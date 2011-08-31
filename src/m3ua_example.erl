-module(m3ua_example).

-include("osmo_util.hrl").
-include("m3ua.hrl").
-include("sccp.hrl").

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
	 m3ua_pid
	}).

init() ->
	% start the M3UA link to the SG
	Opts = [{user_pid, self()}, {sctp_remote_ip, ?T_ST_IP},
            {sctp_remote_port, ?T_ST_PORT},
		{sctp_local_port, ?LOCAL_PORT}, {user_fun, fun m3ua_tx_to_user/2}, {user_args, self()}],
	{ok, M3uaPid} = m3ua_core:start_link(Opts),
	% instantiate SCCP routing instance
	{ok, ScrcPid} = sccp_scrc:start_link([{mtp_tx_action, {callback_fn, fun scrc_tx_to_mtp/2, M3uaPid}}]),
	loop(#loop_dat{m3ua_pid = M3uaPid, scrc_pid = ScrcPid}).

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
	

scrc_tx_to_mtp(Prim, Args) ->
	M3uaPid = Args,
	gen_fsm:send_event(M3uaPid, Prim).

m3ua_tx_to_user(Prim, Args) ->
	UserPid = Args,
	UserPid ! {m3ua_prim, Prim}.


rx_m3ua_prim(#primitive{subsystem = 'M', gen_name = 'SCTP_ESTABLISH', spec_name = confirm}, L) ->
	gen_fsm:send_event(L#loop_dat.m3ua_pid, osmo_util:make_prim('M','ASP_UP',request));

rx_m3ua_prim(#primitive{subsystem = 'M', gen_name = 'ASP_UP', spec_name = confirm}, L) ->
	gen_fsm:send_event(L#loop_dat.m3ua_pid, osmo_util:make_prim('M','ASP_ACTIVE',request));

rx_m3ua_prim(#primitive{subsystem = 'M', gen_name = 'RMT_ASP_ACTIVE', spec_name = confirm}, L) ->
	io:format("Example: M3UA now active and ready~n"),
	tx_sccp_udt(L#loop_dat.scrc_pid);

rx_m3ua_prim(P, _L) ->
	io:format("Example: Ignoring M3UA prim ~p~n", [P]),
	ok.


tx_sccp_udt(ScrcPid) ->
	CallingP = #sccp_addr{ssn = ?SCCP_SSN_MSC, point_code = osmo_util:pointcode2int(itu, {1,2,2})},
	CalledP = #sccp_addr{ssn = ?SCCP_SSN_HLR, point_code = osmo_util:pointcode2int(itu, {1,1,1})},
	Data = <<1,2,3,4>>,
	Opts = [{protocol_class, 0}, {called_party_addr, CalledP},
		{calling_party_addr, CallingP}, {user_data, Data}],
	io:format("Example: Sending N-UNITDATA.req to SCRC~n"),
	gen_fsm:send_event(ScrcPid, osmo_util:make_prim('N','UNITDATA',request,Opts)).

