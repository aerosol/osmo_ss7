% SCCP M3UA / SUA ASP gsn_fsm according to RFC3868 4.3.1

% (C) 2011-2012 by Harald Welte <laforge@gnumonks.org>
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
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(xua_asp_fsm).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(gen_fsm).

-include("osmo_util.hrl").
-include("m3ua.hrl").

% gen_fsm exports
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

% states in this FSM
-export([asp_down/2, asp_inactive/2, asp_active/2]).

% helper functions exporte to callback modules
-export([send_sctp_to_peer/2, send_prim_to_user/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{gen_xua_msg, 3}, {asp_down, 3}, {asp_inactive, 3}, {asp_active, 3}].

% Timeouts in milliseconds
-define(T_ACK_TIMEOUT, 2*60*100).

-record(asp_state, {
		module,
		role,
		t_ack,
		ext_state,
		user_fun,
		user_args,
		sctp_pid
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Module, ModuleArgs, UserFun, UserArgs, SctpPid]) ->
	{ok, ExtState} = Module:init(ModuleArgs),
	AspState = #asp_state{module = Module,
			      user_fun = UserFun,
			      user_args = UserArgs,
			      ext_state = ExtState,
			      sctp_pid = SctpPid,
			      role = asp},
	{ok, asp_down, AspState}.

terminate(Reason, State, _LoopDat) ->
	io:format("Terminating ~p in State ~p (Reason: ~p)~n",
		  [?MODULE, State, Reason]),
	ok.

code_change(_OldVsn, StateName, LoopDat, _Extra) ->
	{ok, StateName, LoopDat}.

handle_event(Event, State, LoopDat) ->
	io:format("Unknown Event ~p in state ~p~n", [Event, State]),
	{next_state, State, LoopDat}.


handle_info(Info, State, LoopDat) ->
	io:format("Unknown Info ~p in state ~p~n", [Info, State]),
	{next_state, State, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "asp_down"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asp_down(#primitive{subsystem = 'M', gen_name = 'ASP_UP',
		    spec_name = request, parameters = _Params}, LoopDat) ->
	% M-ASP_UP.req from user, generate message and send to remote peer
	send_msg_start_tack(LoopDat, asp_down, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPUP, []);
asp_down({timer_expired, t_ack, {?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPUP, Params}}, LoopDat) ->
	send_msg_start_tack(LoopDat, asp_down, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPUP, Params);

asp_down({xua_msg, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPUP_ACK}, LoopDat) ->
	timer:cancel(LoopDat#asp_state.t_ack),
	% transition into ASP_INACTIVE
	send_prim_to_user(LoopDat, osmo_util:make_prim('M','ASP_UP',confirm)),
	{next_state, asp_inactive, LoopDat};

asp_down(WhateverElse, LoopDat = #asp_state{module = Module, ext_state = ExtState}) ->
	Module:asp_down(WhateverElse, ExtState, LoopDat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "asp_inactive"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asp_inactive(#primitive{subsystem = 'M', gen_name = 'ASP_ACTIVE',
			spec_name = request, parameters = Params}, LoopDat) ->
	% M-ASP_ACTIVE.req from user, generate message and send to remote peer
	send_msg_start_tack(LoopDat, asp_inactive, ?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPAC,
			   Params);

asp_inactive({timer_expired, t_ack, {?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPAC, Params}}, LoopDat) ->
	send_msg_start_tack(LoopDat, asp_inactive, ?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPAC, Params);

asp_inactive(#primitive{subsystem = 'M', gen_name = 'ASP_DOWN',
		      spec_name = request, parameters = _Params}, LoopDat) ->
	% M-ASP_DOWN.req from user, generate message and send to remote peer
	send_msg_start_tack(LoopDat, asp_inactive, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN, []);

asp_inactive({timer_expired, t_ack, {?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN, Params}}, LoopDat) ->
	send_msg_start_tack(LoopDat, asp_inactive, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN, Params);

asp_inactive({xua_msg,?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPAC_ACK}, LoopDat) ->
	timer:cancel(LoopDat#asp_state.t_ack),
	% transition into ASP_ACTIVE
	% signal this to the user
	send_prim_to_user(LoopDat, osmo_util:make_prim('M','ASP_ACTIVE',confirm)),
	{next_state, asp_active, LoopDat};

asp_inactive({xua_msg, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN_ACK}, LoopDat) ->
	timer:cancel(LoopDat#asp_state.t_ack),
	% transition into ASP_DOWN
	% signal this to the user
	send_prim_to_user(LoopDat, osmo_util:make_prim('M','ASP_DOWN',confirm)),
	{next_state, asp_down, LoopDat};

asp_inactive(WhateverElse, LoopDat = #asp_state{module = Module, ext_state = ExtState}) ->
	Module:asp_inactive(WhateverElse, ExtState, LoopDat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "asp_active"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asp_active({xua_msg, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN_ACK}, LoopDat) ->
	timer:cancel(LoopDat#asp_state.t_ack),
	% transition into ASP_DOWN
	% signal this to the user
	send_prim_to_user(LoopDat, osmo_util:make_prim('M','ASP_DOWN',confirm)),
	{next_state, asp_down, LoopDat};

asp_active({xua_msg, ?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPIA_ACK}, LoopDat) ->
	timer:cancel(LoopDat#asp_state.t_ack),
	% transition into ASP_INACTIVE
	% signal this to the user
	send_prim_to_user(LoopDat, osmo_util:make_prim('M','ASP_INACTIVE',confirm)),
	{next_state, asp_inactive, LoopDat};

asp_active(#primitive{subsystem = 'M', gen_name = 'ASP_DOWN',
		      spec_name = request, parameters = _Params}, LoopDat) ->
	% M-ASP_DOWN.req from user, generate message and send to remote peer
	send_msg_start_tack(LoopDat, asp_active, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN, []);

asp_active({timer_expired, t_ack, {?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN, Params}}, LoopDat) ->
	send_msg_start_tack(LoopDat, asp_active, ?M3UA_MSGC_ASPSM, ?M3UA_MSGT_ASPSM_ASPDN, Params);

asp_active(#primitive{subsystem = 'M', gen_name = 'ASP_INACTIVE',
		      spec_name = request, parameters = _Params}, LoopDat) ->
	% M-ASP_INACTIVE.req from user, generate message and send to remote peer
	send_msg_start_tack(LoopDat, asp_active, ?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPIA, []);

asp_active({timer_expired, t_ack, {?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPIA, Params}}, LoopDat) ->
	send_msg_start_tack(LoopDat, asp_active, ?M3UA_MSGC_ASPTM, ?M3UA_MSGT_ASPTM_ASPIA, Params);

asp_active(#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		      spec_name = request, parameters = Params}, LoopDat) ->
	% MTP-TRANSFER.req from user app: Send message to remote peer
	OptList = [{?M3UA_IEI_PROTOCOL_DATA, Params}],
	Msg = #m3ua_msg{version = 1, msg_class = ?M3UA_MSGC_TRANSFER,
			msg_type = ?M3UA_MSGT_XFR_DATA,
			payload = OptList},
	send_sctp_to_peer(LoopDat, Msg),
	{next_state, asp_active, LoopDat};
asp_active(#m3ua_msg{version = 1, msg_class = ?M3UA_MSGC_TRANSFER,
		     msg_type = ?M3UA_MSGT_XFR_DATA, payload = Params}, LoopDat) ->
	% Data transfer from remote entity: Send MTP-TRANSFER.ind primitive to the user
	Mtp3 = proplists:get_value(?M3UA_IEI_PROTOCOL_DATA, Params),
	send_prim_to_user(LoopDat, osmo_util:make_prim('MTP','TRANSFER',indication,Mtp3)),
	{next_state, asp_active, LoopDat};

asp_active(WhateverElse, LoopDat = #asp_state{module = Module, ext_state = ExtState}) ->
	Module:asp_active(WhateverElse, ExtState, LoopDat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% helper to send one of the up/down/act/inact management messages + start timer
send_msg_start_tack(LoopDat, State, MsgClass, MsgType, Params) ->
	Module = LoopDat#asp_state.module,
	% generate and send the respective message
	%Msg = #m3ua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType, payload = Params},
	Msg = Module:gen_xua_msg(MsgClass, MsgType, Params),
	send_sctp_to_peer(LoopDat, Msg),
	% start T(ack) timer and wait for ASP_UP_ACK
	timer:cancel(LoopDat#asp_state.t_ack),
	{ok, Tack} = timer:apply_after(?T_ACK_TIMEOUT, gen_fsm, send_event,
				 [self(), {timer_expired, t_ack, {MsgClass, MsgType, Params}}]),
	{next_state, State, LoopDat#asp_state{t_ack = Tack}}.


send_prim_to_user(LoopDat, Prim) when is_record(LoopDat, asp_state),
				      is_record(Prim, primitive) ->
	#asp_state{user_fun = Fun, user_args = Args} = LoopDat,
	Fun(Prim, Args).


% Helper function to send data to the SCTP peer
send_sctp_to_peer(LoopDat, Msg) ->
	Prim = osmo_util:make_prim('MTP','TRANSFER',request, Msg),
	gen_fsm:send_event(LoopDat#asp_state.sctp_pid, Prim).
