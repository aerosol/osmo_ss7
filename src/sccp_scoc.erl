% ITU-T Q.71x SCCP Connection-oriented Control (SCOC)

% (C) 2010 by Harald Welte <laforge@gnumonks.org>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(sccp_scoc).
-behaviour(gen_fsm).

-include("sccp.hrl").

-export([start_link/1]).

-export([init/1, handle_event/3]).
-export([idle/2, conn_pend_in/2, conn_pend_out/2, active/2, disconnect_pending/2,
	 reset_incoming/2, reset_outgoing/2, bothway_reset/2, wait_conn_conf/2]).

-export([make_prim/4, make_prim/3]).

%% gen_fsm callbacks

% Appendix C.4 of Q.714 (all in milliseconds)
-define(CONNECTION_TIMER,	1  *60*100).
-define(TX_INACT_TIMER,		5  *60*100).
-define(RX_INACT_TIMER,		11 *60*100).
-define(RELEASE_TIMER,		10 *100).
-define(RELEASE_REP_TIMER,	10 *100).
-define(INT_TIMER,		1  *60*100).
-define(GUARD_TIMER,		23 *60*100).
-define(RESET_TIMER,		10 *100).
-define(REASSEMBLY_TIMER,	10 *60*100).

-record(state, {
	  role,			% client | server
	  user_application,	% {MonitorRef, pid()}
	  scrc_pid,		% pid()
	  rx_inact_timer,	% TRef
	  tx_inact_timer,	% TRef
	  local_reference,
	  remote_reference,
	  class,
	  user_pid		% pid()
	}).

% TODO: 
% 	expedited data
%	class 3
%	segmentation / reassembly

start_link(InitOpts) ->
	gen_fsm:start_link(sccp_scoc, InitOpts, [{debug, [trace]}]).

init(InitOpts) ->
	LoopDat = #state{user_pid=proplists:get_value(user_pid, InitOpts),
			 scrc_pid=proplists:get_value(scrc_pid, InitOpts),
			 local_reference=proplists:get_value(local_reference, InitOpts)},
	io:format("SCOC init Pid=~p LoopDat ~p~n", [self(), LoopDat]),
	{ok, idle, LoopDat}.

handle_event(stop, _StateName, LoopDat) ->
	io:format("SCOC received stop event~n"),
	{stop, normal, LoopDat};
handle_event({timer_expired, tx_inact_timer}, State, LoopDat) ->
	% FIXME: T(ias) is expired, send IT message
	io:format("FIXME: T(ias) is expired, send IT message~n", []),
	{next_state, State, LoopDat};
handle_event({timer_expired, rx_inact_timer}, State, LoopDat) ->
	io:format("FIXME: T(iar) is expired, release connection~n", []),
	% FIXME: Initiate connection release procedure
	{next_state, State, LoopDat}.

% helper function to create a #primitive record
make_prim(Subsys, GenName, SpecName) ->
	make_prim(Subsys, GenName, SpecName, []).
make_prim(Subsys, GenName, SpecName, Param) ->
	#primitive{subsystem = Subsys, gen_name = GenName,
		   spec_name = SpecName, parameters = Param}.

% helper function to send a primitive to the user
send_user(_LoopDat = #state{user_pid = Pid}, Prim = #primitive{}) ->
	Pid ! {sccp, Prim}.

% low-level functions regarding activity timers
restart_tx_inact_timer(LoopDat) ->
	Tias = timer:apply_after(?TX_INACT_TIMER, gen_fsm, send_all_state_event,
				 [self(), {timer_expired, tx_inact_timer}]),
	LoopDat#state{tx_inact_timer = Tias}.

restart_rx_inact_timer(LoopDat) ->
	Tiar = timer:apply_after(?RX_INACT_TIMER, gen_fsm, send_all_state_event,
				 [self(), {timer_expired, rx_inact_timer}]),
	LoopDat#state{rx_inact_timer = Tiar}.
	
start_inact_timers(LoopDat) ->
	Tias = timer:apply_after(?TX_INACT_TIMER, gen_fsm, send_all_state_event,
				 [self(), {timer_expired, tx_inact_timer}]),
	Tiar = timer:apply_after(?RX_INACT_TIMER, gen_fsm, send_all_state_event,
				 [self(), {timer_expired, rx_inact_timer}]),
	LoopDat#state{rx_inact_timer = Tiar, tx_inact_timer = Tias}.

stop_inact_timers(#state{rx_inact_timer = Tiar, tx_inact_timer = Tias}) ->
	timer:cancel(Tiar),
	timer:cancel(Tias).


% -spec idle(#primitive{} | ) -> gen_fsm_state_return().

% STATE Idle

% N-CONNECT.req from user
idle(#primitive{subsystem = 'N', gen_name = 'CONNECT',
	        spec_name = request, parameters = Param}, LoopDat) ->
	% assign local reference and SLS
	% determine protocol class and credit
	LoopDat1 = LoopDat#state{local_reference = make_ref(), class = 2},
	gen_fsm:send_event(LoopDat1#state.scrc_pid,
			   make_prim('OCRC','CONNECTION', indication, Param)),
	% start connection timer
	{next_state, conn_pend_out, LoopDat1, ?CONNECTION_TIMER};

% RCOC-CONNECTION.req from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION',
		spec_name = indication, parameters = Params}, LoopDat) ->
	% associate remote reference to connection section
	RemRef = proplists:get_value(src_local_ref, Params),
	% determine protocol class and FIXME: credit
	Class = proplists:get_value(protocol_class, Params),
	LoopDat1 = LoopDat#state{remote_reference = RemRef, class = Class},
	% send N-CONNECT.ind to user
	send_user(LoopDat1, make_prim('N', 'CONNECT', indication, [{scoc_pid, self()}|Params])),
	%#primitive{subsystem = 'N', gen_name = 'CONNECT', spec_name = indication}
	{next_state, conn_pend_in, LoopDat1};

% RCOC-ROUTING_FAILURE.ind from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'ROUTING FAILURE',
		spec_name = indication}, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'CONNECTION REFUSED', indication)),
	{next_state, idle, LoopDat};

%FIXME: request type 2 ?!?

% RCOC-RELEASED.ind from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'RELEASED',
		spec_name = indication}, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASE COMPLETE', indication)),
	{next_state, idle, LoopDat};

% RCOC-RELEASE_COMPLETE.ind from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'RELEASE COMPLETE',
		spec_name = indication}, LoopDat) ->
	{next_state, idle, LoopDat};

idle(#primitive{subsystem= 'RCOC', gen_name = 'DATA',
		spec_name = indication, parameters = Param}, LoopDat) ->
	% FIXME: if source reference, send error
	send_user(LoopDat, make_prim('N', 'DATA', indication, Param)),
	{next_state, idle, LoopDat}.

% STATE Connection pending incoming
conn_pend_in(#primitive{subsystem = 'N', gen_name = 'CONNECT',
			spec_name = response, parameters = Param}, LoopDat) ->
	io:format("SCOC N-CONNECT.resp LoopDat ~p~n", [LoopDat]),
	% assign local reference, SLS, protocol class and credit for inc section
	OutParam = [{dst_local_ref, LoopDat#state.remote_reference},
		    {src_local_ref, LoopDat#state.local_reference},
		    {protocol_class, LoopDat#state.class}] ++ Param,
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'CONNECTION', confirm, OutParam)),
	% start inactivity timers
	LoopDat1 = start_inact_timers(LoopDat),
	{next_state, active, LoopDat1};
conn_pend_in(any_npdu_type, LoopDat) ->
	{next_state, conn_pend_in, LoopDat};
conn_pend_in(#primitive{subsystem = 'N', gen_name = 'DISCONNECT',
			spec_name = request, parameters = Param}, LoopDat) ->
	% release resourcers (local ref may have to be released an frozen)
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'CONNECTION REFUSED', indication, Param)),
	{next_state, idle, LoopDat}.
	

disc_ind_stop_rel_3(LoopDat) ->
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, make_prim('N', 'DISCONNECT',indication)),
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASED', indication)),
	% start release timer
	{next_state, disconnect_pending, LoopDat, ?RELEASE_TIMER}.

rel_res_disc_ind_idle_2(LoopDat) ->
	% release resources and local reference (freeze)
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, make_prim('N', 'DISCONNECT', indication)),
	{next_state, idle, LoopDat}.


% STATE Connection pending outgoing
conn_pend_out(#primitive{subsystem = 'N', gen_name = 'DISCONNECT',
			 spec_name = request}, LoopDat) ->
	% FIXME: what about the connection timer ?
	{next_state, wait_conn_conf, LoopDat};
conn_pend_out(timeout, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat);
conn_pend_out(routing_failure, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat);
conn_pend_out(released, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC',  'RELEASE COMPLETE', indication)),
	rel_res_disc_ind_idle_2(LoopDat);
% other N-PDU Type
conn_pend_out(other_npdu_type, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat);
conn_pend_out(connection_refused, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat);
conn_pend_out(connection_confirm, LoopDat) ->
	% start inactivity timers
	LoopDat1 = start_inact_timers(LoopDat),
	% assign protocol class and associate remote reference to connection
	% send N-CONNECT.conf to user
	send_user(LoopDat1, #primitive{subsystem = 'N', gen_name = 'CONNECT',
				       spec_name = confirm}),
	{next_state, active, LoopDat1}.

stop_c_tmr_rel_idle_5(LoopDat) ->
	% stop connection timer (implicit)
	% release resources and local reference
	{next_state, idle, LoopDat}.

rel_freeze_idle(LoopDat) ->
	{next_state, idle, LoopDat}.

% STATE Wait connection confirmed
wait_conn_conf(released, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASE COMPLETE', indication)),
	stop_c_tmr_rel_idle_5(LoopDat);
wait_conn_conf(connection_confirm, LoopDat) ->
	% stop connection timer (implicit)
	% associate remote reference to connection
	relsd_tmr_disc_pend_6(LoopDat);
wait_conn_conf(other_npdu_type, LoopDat) ->
	% stop connection timer (implicit)
	rel_freeze_idle(LoopDat);
wait_conn_conf(timeout, LoopDat) ->
	stop_c_tmr_rel_idle_5(LoopDat);
wait_conn_conf(connection_refused, LoopDat) ->
	stop_c_tmr_rel_idle_5(LoopDat);
wait_conn_conf(routing_failure, LoopDat) ->
	stop_c_tmr_rel_idle_5(LoopDat).


relsd_tmr_disc_pend_6(LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASED', indication)),
	% start release timer
	{next_state, disconnect_pending, LoopDat, ?RELEASE_TIMER}.

% STATE Active
active(#primitive{subsystem = 'N', gen_name = 'DISCONNECT',
		  spec_name = request}, LoopDat) ->
	% stop inactivity timers
	start_inact_timers(LoopDat),
	relsd_tmr_disc_pend_6(LoopDat);
active(internal_disconnect, LoopDat) ->
	disc_ind_stop_rel_3(LoopDat);
active(connection_refused, LoopDat) ->
	{next_state, active, LoopDat};
active(connection_confirm, LoopDat) ->
	{next_state, active, LoopDat};
active(release_complete, LoopDat) ->
	{next_state, active, LoopDat};
active(released, LoopDat) ->
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'DISCONNECT',
				      spec_name = indication}),
	% release resources and local reference (freeze)
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASE COMPLETE', indication)),
	{next_state, idle, LoopDat};
active(error, LoopDat) ->
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'DISCONNECT',
				      spec_name = indication}),
	% release resources and local reference (freeze)
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASE COMPLETE', indication)),
	{next_state, idle, LoopDat};
active(rcv_inact_tmr_exp, LoopDat) ->
	disc_ind_stop_rel_3(LoopDat);
active(routing_failure, LoopDat) ->
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'DISCONNECT',
				      spec_name = indication}),
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	% start release timer
	{next_state, disconnect_pending, LoopDat, ?RELEASE_TIMER};
% Connection release procedures at destination node
%active(internal_disconnect) ->
% Data transfer procedures
active(#primitive{subsystem = 'N', gen_name = 'DATA',
		  spec_name = request, parameters = Param}, LoopDat) ->
	% FIXME Segment NSDU and assign value to bit M
	% FIXME handle protocol class 3
	gen_fsm:send_event(LoopDat#state.scrc_pid, {dt1, []}),
	% restart send inactivity timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	{next_state, active, LoopDat1};
active(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		  spec_name = indication, parameters = MsgPrim}, LoopDat) ->
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	% FIXME handle protocol class 3
	% FIXME check for M-bit=1 and put data in Rx queue
	% N-DATA.ind to user
	UserData = proplists:get_value(user_data, MsgPrim#primitive.parameters),
	send_user(LoopDat, make_prim('N', 'DATA', indication, {user_data, UserData})),
	{next_state, active, LoopDat1};
% Reset procedures
active(#primitive{subsystem = 'N', gen_name = 'RESET',
		  spec_name = request, parameters = Param}, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RESET', request, Param)),
	% start reset timer
	% restart send inact timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	% reset variables and discard all queued and unacked msgs
	{next_state, reset_outgoing, LoopDat1, ?RESET_TIMER};
active(internal_reset_req, LoopDat) ->
	% N-RESET.ind to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'RESET',
				      spec_name = indication}),
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RESET', request)),
	% start reset timer
	% restart send inact timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	% reset variables and discard all queued and unacked msgs
	{next_state, bothway_reset, LoopDat1, ?RESET_TIMER};
active(reset_confirm, LoopDat) ->
	% discard received message
	{next_state, active, LoopDat};
active(reset_req, LoopDat) ->
	% restart send inactivity timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	% N-RESET.ind to user
	send_user(LoopDat, make_prim('N', 'RESET', indication)),
	% reset variables and discard all queued and unacked msgs
	{next_state, reset_incoming, LoopDat1}.

rel_res_stop_tmr_12(LoopDat) ->
	% release resources and local reference (freeze)
	% stop release and interval timers
	{next_state, idle, LoopDat}.

% STATE Disconnect pending
disconnect_pending(release_complete, LoopDat) ->
	rel_res_stop_tmr_12(LoopDat);
disconnect_pending(released_error, LoopDat) ->
	rel_res_stop_tmr_12(LoopDat);
disconnect_pending(routing_failure, LoopDat) ->
	{next_state, disconnect_pending};
disconnect_pending(other_npdu_type, LoopDat) ->
	% discared received message
	{next_state, disconnect_pending};
disconnect_pending(timeout, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASED', indication)),
	% start interval timer
	% FIXME start repeat release timer
	{next_state, disconnect_pending, ?RELEASE_REP_TIMER};
disconnect_pending(intv_tmr_exp, LoopDat) ->
	% inform maintenance
	rel_res_stop_tmr_12(LoopDat);
% FIXME: this is currently ending up in normal 'timeout' above
disconnect_pending(repeat_release_tmr_exp, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   make_prim('OCRC', 'RELEASED', indication)),
	% FIXME restart repeat release timer
	{next_state, disconnect_pending}.

res_out_res_conf_req(LoopDat) ->
	% N-RESET.conf to user
	send_user(LoopDat, make_prim('N', 'RESET', confirm)),
	% stop reset timer (implicit)
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	% resume data transfer
	{next_state, active, LoopDat1}.

% STATE Reset outgoing
reset_outgoing(#primitive{subsystem = 'N', gen_name = 'DATA',
			  spec_name = request, parameters = Params}, LoopDat) ->
	% FIXME received information ?!?
	{next_state, reset_outgoing, LoopDat};
reset_outgoing(#primitive{subsystem = 'N', gen_name = 'EXPEDITED DATA',
			  spec_name = request, parameters = Params}, LoopDat) ->
	% FIXME received information ?!?
	{next_state, reset_outgoing, LoopDat};
reset_outgoing(timeout, LoopDat) ->
	% FIXME check for temporary connection section
	% inform maintenance
	{next_state, maintenance_Blocking, LoopDat};
%reset_outgoing(error, LoopDat) ->
%reset_outgoing(released, LoopDat) ->
reset_outgoing(other_npdu_type, LoopDat) ->
	% discard received message
	{next_state, reset_outgoing, LoopDat};
reset_outgoing(reset_confirm, LoopDat) ->
	res_out_res_conf_req(LoopDat);
reset_outgoing(reset_request, LoopDat) ->
	res_out_res_conf_req(LoopDat).

bway_res_req_resp(LoopDat) ->
	{next_state, reset_outgoing, LoopDat}.

bway_res_res_conf_req(LoopDat) ->
	% N-RESET.conf to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'RESET',
				      spec_name = confirm}),
	% stop reset timer (implicit)
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	{next_state, reset_incoming, LoopDat1}.

% STATE Bothway Reset
bothway_reset(#primitive{subsystem = 'N', gen_name = 'RESET',
			 spec_name = request, parameters = Params}, LoopDat) ->
	bway_res_req_resp(LoopDat);
bothway_reset(#primitive{subsystem = 'N', gen_name = 'RESET',
			 spec_name = response, parameters = Params}, LoopDat) ->
	bway_res_req_resp(LoopDat);
bothway_reset(timeout, LoopDat) ->
	% FIXME check for temporary connection section
	% inform maintenance
	{next_state, maintenance_Blocking, LoopDat};
%bothway_reset(error, LoopDat) ->
%bothway_reset(released, LoopDat) ->
bothway_reset(other_npdu_type, LoopDat) ->
	% discard received message
	{next_state, bothway_reset, LoopDat}.

% STATE Reset incoming
reset_incoming(#primitive{subsystem = 'N', gen_name = 'RESET',
			  spec_name = request, parameters = Params}, LoopDat) ->
	% received information
	{nest_state, reset_incoming, LoopDat};
%reset_incoming(error, LoopDat) ->
%reset_incoming(released, LoopDat) ->
reset_incoming(other_npdu_type, LoopDat) ->
	% discard received message
	% internal reset request
	{next_state, active, LoopDat}.
% FIXME: response or request
%reset_incoming(
