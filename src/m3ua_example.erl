-module(m3ua_example).

-include("osmo_util.hrl").
-include("m3ua.hrl").
-include("sccp.hrl").
-include("mtp3.hrl").

-export([init/0]).

-record(loop_dat, {
	 scrc_pid,
	 m3ua_pids
	}).

-define(T_ST_PC, osmo_util:pointcode2int(itu, {0,0,1})). %% 1
-define(T_ST_SSN, 147).
-define(T_ST_GT, 48507999970).
-define(T_ST_ROUT_CTX, 1).
-define(LOCAL_PC, osmo_util:pointcode2int(itu, {0, 12, 5})). %% 101
-define(LOCAL_SSN, 123).


%%%%%%%
% API %
%%%%%%%

init() ->
    dpc_map = ets:new(dpc_map, [set, named_table]),
    linksets:start(),
    LinkSets = confetti:fetch(linksets),
    %% One SCRC is enough right now -- callback fn is used by SCRC
    %% to notify lower layers.
    %% I.e. after UDT message is encoded, SCRC sends the encoded
    %% message, it's state and other parameters with send_mtp_transfer_down
    %% function, that will use the callback internally
    {ok, ScrcPid} = sccp_scrc:start_link([
            {mtp_tx_action, {callback_fn, fun scrc_tx_to_mtp/2, []}}]),
    %% Spawn M3UA connections and perform DPC-Link mapping
    M3uaPids = lists:map(fun spawn_m3ua/1, LinkSets),
    loop(#loop_dat{m3ua_pids = M3uaPids, scrc_pid = ScrcPid}).


%%%%%%%%%%%%%
% Internals %
%%%%%%%%%%%%%


%% Loop until receive something that's not an M3UA Primitive
%% Primitives are sent from M3UA Core with send_prim_to_user/2
%% and delivered to the current process with the user_fun callback
%% held in every m3ua_core process state.
%%
%% This happends on certain events, i.e. state change or receieving M3UA from remote peer.
%%
%% Example primitives arriving here:
%%  {primitive,'M','SCTP_ESTABLISH',confirm,[]}
%%                                  - ignored
%%
%%  {primitive,'M','RMT_ASP_ACTIVE',confirm,[]}
%%                                  - triggers Unitdata message
%%                                    sending via SCRC
%%
loop(L) ->
    io:format("Example: Entering main loop~n"),
    receive
            {m3ua_prim, Prim} ->
                    io:format("Example: Rx M3UA Prim ~p~n", [Prim]),
                    rx_m3ua_prim(Prim, L);
            Stop ->
                    io:format("Example: Received ~p~n", [Stop]),
                    exit(stop_received)
    end,
    loop(L).


%% This should be reviewed - we do actually need to support linksets.
%% An M3UA SCTP association between an ASP and an SGP for an AS is
%% equivalent to a link and a group of SCTP associations betweeen
%% ASP and SGP for the same AS and SG is equivalent to a linkset.
%%
spawn_m3ua(_Conn = {Name, [{remote_ip, Ip}, {remote_port, Port},
            {local_port, LPort}, _Assocs, {dpcs, Dpcs}]}) ->
       %% user_fun is called by m3ua_core to send primitives to the user layer
       Opts = [{user_pid, self()},
               {sctp_remote_ip, Ip},
               {sctp_remote_port, Port},
               {sctp_local_port, LPort},
               {user_fun, fun m3ua_tx_to_user/2},
               {user_args, self()},
               {init_state, asp_active}],
        {ok, Pid} = m3ua_core:start_link(Opts),
        lists:foreach(fun(Dpc) ->
                    ets:insert(dpc_map, {Dpc, Pid})
            end, Dpcs),
        io:format("=== M3UACORE ~p ~p~n", [Name, Pid]),
        Pid.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SCRC & M3UA Callback functions %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% SCRC generates MTP-TRANSFER.req and lets user layer decide what's next
%% We are going to (temporarily) lookup DPC mapping table and pass the transfer
%% message to the corresponding M3UA. Nevertheless this should be probably done
%% within SCRC, but that's for further investigation - TODO
%%
scrc_tx_to_mtp(Prim = #primitive{subsystem = 'MTP', gen_name='TRANSFER', spec_name=request,
        parameters = Mtp3Msg}, _A) when is_record(Mtp3Msg, mtp3_msg) ->
    %% get destination point code
    DPC = Mtp3Msg#mtp3_msg.routing_label#mtp3_routing_label.dest_pc,
    io:format("Got destination point code: ~p~n", [DPC]),
    case ets:lookup(dpc_map, DPC) of
        [{_, M3uaPid}] ->
            io:format("Found DPC mapping for ~p: ~p~n", [DPC, M3uaPid]),
            %% pass it to the corresponding M3UA process.
            %% m3ua_core FSM should be active to be able to handle it and
            %% actually create and M3UA message with Mtp3Msg payload and send
            %% it to the remote peer.
            %% see m3ua_core:asp_active{#primitive{gen_name='TRANSFER'}}
            gen_fsm:send_event(M3uaPid, Prim);
        Other ->
            %% TODO there is probably some message refusal procedure to be
            %% applied here
            io:format("ERROR! Could not find proper DPC ~p mapping.~n", [DPC])
    end.

%% A helper function used by m3ua_core to deliver primitives to the user
%% layer process
%%
m3ua_tx_to_user(Prim, Args) ->
    UserPid = Args,
    UserPid ! {m3ua_prim, Prim}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle M3UA Primitives functions %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rx_m3ua_prim(#primitive{subsystem = 'M', gen_name = 'ASP_ACTIVE', spec_name = confirm}, L) ->
    io:format("Local ASP Active!~n");

%% This is my custom primitive used in client-server model.
%% When remote Application Server is active, we are ready to start UDT message
%% delivery process
%%
rx_m3ua_prim(#primitive{subsystem = 'M', gen_name = 'RMT_ASP_ACTIVE', spec_name = confirm}, L) ->
    io:format("Example: remote peer now active and ready~n"),
    %gen_fsm:send_event(L#loop_dat.m3ua_pid, hack_force_activate),

    CallingP = #sccp_addr{
        ssn = ?LOCAL_SSN,
        point_code = ?LOCAL_PC
    },
    CalledP = #sccp_addr{
        ssn = ?T_ST_SSN,
        point_code = ?T_ST_PC
    },
    Data = <<"message">>,
    tx_sccp_udt(L#loop_dat.scrc_pid, CallingP, CalledP, Data, 1);

rx_m3ua_prim(P, _L) ->
    io:format("Example: Ignoring M3UA prim ~p~n", [P]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Further layer communication functions %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create Unitdata message and pass it to the routing facility
%% There should be GTT involved if needed; And this is TODO
%%
tx_sccp_udt(ScrcPid, CallingP = #sccp_addr{},
            CalledP = #sccp_addr{}, Data, ProtoClass)
            when is_binary(Data), is_integer(ProtoClass) ->
                Opts = [{protocol_class, {ProtoClass, 0}}, {called_party_addr, CalledP},
                        {calling_party_addr, CallingP}, {user_data, Data}],
                io:format("Example: Sending N-UNITDATA.req to SCRC~n"),
                gen_fsm:send_event(ScrcPid, osmo_util:make_prim('N','UNITDATA',request,Opts)).

