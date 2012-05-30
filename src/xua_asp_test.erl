
-module(xua_asp_test).

-include("osmo_util.hrl").
-include("m3ua.hrl").

-export([start/0, create_asp/0]).
-export([asp_up_ack/1, asp_active_ack/1, asp_inactive_ack/1, asp_down_ack/1]).

start() ->
	sg_as_sup:start_link("foo", [{debug, [trace]}]).

create_asp() ->
	Fun = fun(Prim, Args) -> asp_prim_to_user(Prim, Args) end,
	AspArgs = [sua_asp, [], Fun, [], self()],
	xua_as_fsm:create_asp("foo", AspArgs).

asp_up_ack(Pid) ->
	gen_fsm:send_event(Pid, {xua_msg, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPUP_ACK}).

asp_active_ack(Pid) ->
	gen_fsm:send_event(Pid, {xua_msg,?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPAC_ACK}).

asp_inactive_ack(Pid) ->
	gen_fsm:send_event(Pid, {xua_msg, ?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPIA_ACK}).

asp_down_ack(Pid) ->
	gen_fsm:send_event(Pid, {xua_msg, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN_ACK}).


asp_prim_to_user(Prim, _Args) ->
	io:format("ASP->SCTP: ~p~n", [Prim]).
