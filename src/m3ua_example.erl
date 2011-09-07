-module(m3ua_example).

-include("osmo_util.hrl").
-include("m3ua.hrl").
-include("sccp.hrl").
-include("mtp3.hrl").

-define(T_ST_PC, osmo_util:pointcode2int(itu, {0,0,1})). %% 1
-define(T_ST_SSN, 147).
-define(T_ST_GT, 48507999970).
-define(T_ST_IP, {10,16,201,62}).
-define(T_ST_PORT, 2906).
-define(T_ST_ROUT_CTX, 1).
-define(LOCAL_IP, {10,16,201,1}).
-define(LOCAL_PORT, 2905).
-define(LOCAL_PC, osmo_util:pointcode2int(itu, {0, 12, 5})). %% 101
-define(LOCAL_SSN, 123).


-export([init/0]).

-record(loop_dat, {
	 scrc_pid,
	 m3ua_pids
	}).

spawn_m3ua(Conn = {Name, [{remote_ip, Ip}, {remote_port, Port},
            {local_port, LPort}, _Assocs, {dpcs, Dpcs}]}) ->


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



init() ->
    dpc_map = ets:new(dpc_map, [set, named_table]),
    linksets:start(),
    LinkSets = confetti:fetch(linksets),
    M3uaPids = lists:map(fun spawn_m3ua/1, LinkSets),
    {ok, ScrcPid} = sccp_scrc:start_link([
            {mtp_tx_action, {callback_fn, fun scrc_tx_to_mtp/2, []}}]),
    loop(#loop_dat{m3ua_pids = M3uaPids, scrc_pid = ScrcPid}).

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

scrc_tx_to_mtp(Prim = #primitive{subsystem = 'MTP', gen_name='TRANSFER', spec_name=request,
        parameters = Mtp3Msg}, _A) when is_record(Mtp3Msg, mtp3_msg) ->
    DPC = Mtp3Msg#mtp3_msg.routing_label#mtp3_routing_label.dest_pc,
    io:format("*** Got destination point code: ~p~n", [DPC]),
    case ets:lookup(dpc_map, DPC) of
        [{_, M3uaPid}] ->
            io:format("Found DPC mapping for ~p: ~p~n", [DPC, M3uaPid]),
            gen_fsm:send_event(M3uaPid, Prim);
        Other ->
            io:format("ERROR! Could not find proper DPC ~p mapping.~n", [DPC])
    end.

m3ua_tx_to_user(Prim, Args) ->
    UserPid = Args,
    UserPid ! {m3ua_prim, Prim}.

rx_m3ua_prim(#primitive{subsystem = 'M', gen_name = 'ASP_ACTIVE', spec_name = confirm}, L) ->
    io:format("Local ASP Active!~n");

rx_m3ua_prim(#primitive{subsystem = 'M', gen_name = 'RMT_ASP_ACTIVE', spec_name = confirm}, L) ->
    io:format("Example: remote peer now active and ready~n"),
    %gen_fsm:send_event(L#loop_dat.m3ua_pid, hack_force_activate),
    tx_sccp_udt(L#loop_dat.scrc_pid);

rx_m3ua_prim(P, _L) ->
    io:format("Example: Ignoring M3UA prim ~p~n", [P]),
    ok.

tx_sccp_udt(ScrcPid) ->
    CallingP = #sccp_addr{ssn = ?SCCP_SSN_MSC, point_code = osmo_util:pointcode2int(itu, {1,2,2})},
    CalledP = #sccp_addr{ssn = ?SCCP_SSN_HLR, point_code = osmo_util:pointcode2int(itu, {1,1,1})},
    Data = <<100,6,73,4,53,33,191,30>>,
    Opts = [{protocol_class, {0, 0}}, {called_party_addr, CalledP},
            {calling_party_addr, CallingP}, {user_data, Data}],
    io:format("Example: Sending N-UNITDATA.req to SCRC~n"),
    gen_fsm:send_event(ScrcPid, osmo_util:make_prim('N','UNITDATA',request,Opts)).

