% M2UA / M3UA / SUA AS gsn_fsm according to RFC3868 4.3.1

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

-module(xua_as_fsm).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(gen_fsm).

-include("osmo_util.hrl").
-include("m3ua.hrl").

% gen_fsm exports
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

% states in this FSM
-export([as_down/2, as_inactive/2, as_active/2, as_pending/2]).

% Timeouts in milliseconds
-define(T_R_TIMEOUT, 2*60*100).

-record(as_state, {
		role,
		t_r,
		asp_list
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	AsState = #as_state{asp_list = [],
			    role = sg},
	{ok, as_down, AsState}.

terminate(Reason, State, _LoopDat) ->
	io:format("Terminating ~p in State ~p (Reason: ~p)~n",
		  [?MODULE, State, Reason]),
	ok.

code_change(_OldVsn, StateName, LoopDat, _Extra) ->
	{ok, StateName, LoopDat}.

handle_event(Event, State, LoopDat) ->
	io:format("Unknown Event ~p in state ~p~n", [Event, State]),
	{next_state, State, LoopDat}.

handle_info({'EXIT', Pid, Reason}, State, LoopDat) ->
	io:format("EXIT from Process ~p (~p), cleaning up ASP list~n",
		  [Pid, Reason]),
	% FIXME: send fake ASP-DOWN event about ASP to self
	{next_state, State, LoopDat};

handle_info(Info, State, LoopDat) ->
	io:format("Unknown Info ~p in state ~p~n", [Info, State]),
	{next_state, State, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "as_down"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

as_down(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_INACTIVE',
		   spec_name = indication, parameters = _Params}, LoopDat) ->
	% One ASP transitions into ASP-INACTIVE
	next_state(as_inactive, LoopDat);

as_down(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_DOWN',
		   spec_name = indication, parameters = _Params}, LoopDat) ->
	% ignore
	next_state(as_down, LoopDat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "as_inactive"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

as_inactive(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_DOWN',
			spec_name = indication, parameters = AsPid}, LoopDat) ->
	% One ASP transitions into ASP-DOWN
	% FIXME: check if there are any other ASP != DOWN, if yes -> as_inactive
	case check_any_other_asp_not_down(LoopDat, AsPid) of
		true ->
			next_state(as_inactive, LoopDat);
		false ->
			next_state(as_down, LoopDat)
	end;

as_inactive(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_ACTIVE',
			spec_name = indication, parameters = Params}, LoopDat) ->
	% One ASP transitions to ASP-ACTIVE
	next_state(as_active, LoopDat);

as_inactive(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_INACTIVE',
		       spec_name = indication, parameters = _Params}, LoopDat) ->
	% ignore
	next_state(as_inactive, LoopDat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "as_active"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

as_active(#primitive{subsystem = 'ASPAS', gen_name = InactDown,
		     spec_name = indication, parameters = AspPid}, LoopDat) when
		InactDown == 'ASP_DOWN'; InactDown == 'ASP_INACTIVE' ->
	% One ASP transitions to ASP-INACTIVE
	% check if there are other ASP in active, if yes -> as_active
	case check_any_other_asp_in_active(LoopDat, AspPid) of
		true ->
			next_state(as_active, LoopDat);
		false ->
			{ok, Tr} = timer:apply_after(?T_R_TIMEOUT, gen_fsm, send_event,
						 [self(), {timer_expired, t_r}]),
			next_state(as_pending, LoopDat#as_state{t_r = Tr})
	end;

as_active(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_ACTIVE',
		     spec_name = indication, parameters = _Params}, LoopDat) ->
	% ignore
	next_state(as_active, LoopDat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "as_pending"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

as_pending(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_ACTIVE',
		      spec_name = indication}, LoopDat) ->
	% One ASP transitions into ASP-ACTIVE
	timer:cancel(LoopDat#as_state.t_r),
	next_state(as_active, LoopDat);

as_pending(#primitive{subsystem = 'ASPAS', gen_name = 'ASP_INACTIVE',
		      spec_name = indication, parameters = _Params}, LoopDat) ->
	% ignore
	next_state(as_pending, LoopDat);

% FIXME: do we need to re-check as_pending state if we get ASP_DOWN of the last
% inactive ASP ?

as_pending({timer_expired, t_r}, LoopDat) ->
	% check if there is at least one ASP in ASP-INACTIVE -> AS-INACTIVE
	case check_any_other_asp_in_inactive(LoopDat, undefined) of
		true ->
			next_state(as_inactive, LoopDat);
		false ->
			next_state(as_down, LoopDat)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_state(NewState, LoopDat) ->
	%FIXME Module:as_state_change(NewState, LoopDat#as_state.ext_state),
	{next_state, NewState, LoopDat}.

%create_asp(LoopDatIn = #as_state{asp_module = {AspModule, AspModuleArgs},
%				 asp_list = AspListIn}) ->
%	Args = [AspModule, AspModuleArgs, UserFun, UserFunArgs, SctpPid],
%	{ok, AspPid} = gen_fsm:start_link(xua_asp_fsm, Args, [{debug, [trace]}]),
%	{AspPid, LoopDatIn#{asp_list = [AspPid|AspListIn]}}.



check_any_other_asp_in_inactive(LoopDat, AspPid) ->
	check_any_other_asp_in_state('ASP_INACTIVE', LoopDat, AspPid).

check_any_other_asp_in_active(LoopDat, AspPid) ->
	check_any_other_asp_in_state('ASP_ACTIVE', LoopDat, AspPid).

check_any_other_asp_not_down(LoopDat, AspPid) ->
	ListWithoutMe = lists:delete(AspPid, LoopDat#as_state.asp_list),
	StateList = build_asp_state_list(ListWithoutMe),
	not lists:all('ASP_DOWN', StateList).

check_any_other_asp_in_state(State, LoopDat, AspPid) ->
	ListWithoutMe = lists:delete(AspPid, LoopDat#as_state.asp_list),
	StateList = build_asp_state_list(ListWithoutMe),
	lists:member(State, StateList).

build_asp_state_list(ListOfPids) ->
	% FIXME
	[].
