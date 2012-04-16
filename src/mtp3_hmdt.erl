% MTP3 Message handling; message distribution (HMDT) according to Q.704

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

-module(mtp3_hmdt).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(gen_fsm).

-include("mtp3.hrl").

% gen_fsm exports 
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

% individual FSM states
-export([idle/2, own_sp_restart/2]).

-record(hmdt_state, {
		sltc_pid
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Sltc]) when is_pid(Sltc) ->
	HmdtState = #hmdt_state{sltc_pid = Sltc},
	{ok, idle, HmdtState}.

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

% See Figure 2 of Q.707

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: idle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

idle(M=#mtp3_msg{service_ind=Sio}, LoopDat) ->
	handle_mtp3(Sio, M, LoopDat),
	{next_state, idle, LoopDat};

idle(restart_begins, LoopDat) ->
	{next_state, own_sp_restart, LoopDat}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: own_sp_restart
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

own_sp_restart(M=#mtp3_msg{service_ind=Sio}, LoopDat) when
			Sio == ?MTP3_SERV_MGMT; Sio == ?MTP3_SERV_MTN ->
	handle_mtp3(Sio, M, LoopDat),
	{next_state, own_sp_restart, LoopDat};

own_sp_restart(restart_ends, LoopDat) ->
	{next_state, idle, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_mtp3(?MTP3_SERV_MTN, Mtp3, LoopDat) ->
	io:format("SIO ~p HMDT -> SLTC~n", [?MTP3_SERV_MTN]),
	gen_fsm:send_event(LoopDat#hmdt_state.sltc_pid, Mtp3);
handle_mtp3(?MTP3_SERV_MGMT, Mtp3, LoopDat) ->
	io:format("SIO ~p HMDT -> NULL~n", [?MTP3_SERV_MGMT]),
	% FIXME: distinguish between SRM, SLM and STM
	ok;
handle_mtp3(Sio, Mtp3, LoopDat) ->
	io:format("SIO ~p HMDT -> ss7_links~n", [Sio]),
	% deliver to subsystem
	ss7_links:mtp3_rx(Mtp3),
	% FIXME: Send UPU! ?
	ok.
