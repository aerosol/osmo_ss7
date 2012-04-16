% MTP2 Initial Alignment Control according to Q.703 Figure 4 / Figure 9

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

-module(mtp2_iac).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(gen_fsm).

% gen_fsm exports 
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

% states in this FSM
-export([idle/2, not_aligned/2, aligned/2, proving/2]).

% Timeouts in milliseconds According to Q.703 / Section 12.3
-define(M2PA_T1_DEF,	 50000).
-define(M2PA_T2_DEF,	150000).
-define(M2PA_T3_DEF,	  2000).
-define(M2PA_T4N_DEF,	  8200).
-define(M2PA_T4E_DEF,	   500).

-record(iac_state, {
		t2_timeout,
		t3_timeout,
		t4_timeout,
		t4_timeout_pn,
		t4_timeout_pe,
		t2, t3, t4,
		emergency,
		cp,
		further_prov,
		lsc_pid,
		aerm_pid,
		txc_pid
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Lsc, Aerm, Txc]) ->
	IacState = #iac_state{t2_timeout = ?M2PA_T2_DEF,
			      t3_timeout = ?M2PA_T3_DEF,
			      t4_timeout_pn = ?M2PA_T4N_DEF,
			      t4_timeout_pe = ?M2PA_T4E_DEF,
			      emergency = 0,
			      cp = 0,
			      further_prov = 1,
			      lsc_pid = Lsc,
			      aerm_pid = Aerm,
			      txc_pid = Txc},
	{ok, idle, IacState}.

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
% STATE "idle"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

idle(start, LoopDat) ->
	% send sio
	send_to_txc(si_o, LoopDat),
	% start timer
	T2tout = LoopDat#iac_state.t2_timeout,
	{ok, T2} = timer:apply_after(T2tout, gen_fsm, send_event,
			  	     [self(), {timer_expired, t2}]),
	{next_state, not_aligned, LoopDat#iac_state{t2 = T2}};
idle(emergency, LoopDat) ->
	% mark emergency
	{next_state, idle, LoopDat#iac_state{emergency = 1}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "not aligned"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_aligned(stop, LoopDat) ->
	% stop T2
	timer:cancel(LoopDat#iac_state.t2),
	% cancel emergency
	{next_state, idle, LoopDat#iac_state{emergency=0}};
not_aligned(si_e, LoopDat) ->
	% stop T2
	timer:cancel(LoopDat#iac_state.t2),
	T4tout = LoopDat#iac_state.t4_timeout_pe,
	% send SIE or SIN
	case LoopDat#iac_state.emergency of
		0 ->
			Send = si_n;
		_ ->
			Send = si_e
	end,
	send_to_txc(Send, LoopDat),
	% start T3
	T3tout = LoopDat#iac_state.t3_timeout,
	{ok, T3} = timer:apply_after(T3tout, gen_fsm, send_event,
				     [self(), {timer_expired, t3}]),
	{next_state, aligned, LoopDat#iac_state{t3 = T3, t2 = undefined, t4_timeout = T4tout}};
not_aligned(What, LoopDat) when What == si_o; What == si_n ->
	% stop T2
	timer:cancel(LoopDat#iac_state.t2),
	% send SIE or SIN
	case LoopDat#iac_state.emergency of
		0 ->
			T4tout = LoopDat#iac_state.t4_timeout_pn,
			Send = si_n;
		_ ->
			T4tout = LoopDat#iac_state.t4_timeout_pe,
			Send = si_e
	end,
	send_to_txc(Send, LoopDat),
	T3tout = LoopDat#iac_state.t3_timeout,
	{ok, T3} = timer:apply_after(T3tout, gen_fsm, send_event,
				     [self(), {timer_expired, t3}]),
	{next_state, aligned, LoopDat#iac_state{t3 = T3, t2 = undefined, t4_timeout = T4tout}};
not_aligned(emergency, LoopDat) ->
	% mark emergency
	{next_state, not_aligned, LoopDat#iac_state{emergency=1}};
not_aligned(si_os, LoopDat) ->
	% ignore SIOS in this state, as some implementations (notably
	% yate) seem to send it in violation of the spec
	{next_state, not_aligned, LoopDat};
not_aligned({timer_expired, t2}, LoopDat) ->
	% send 'alignment not possible' to LSC
	send_to_lsc(alignment_not_possible, LoopDat),
	% stop emergency
	{next_state, idle, LoopDat#iac_state{emergency=0}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "aligned"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
aligned(What, LoopDat) when What == si_n; What == si_e ->
	case What of
		si_e ->
			% set T4 to Pe
			T4tout = LoopDat#iac_state.t4_timeout_pe;
		_ ->
			T4tout = LoopDat#iac_state.t4_timeout_pn
	end,
	% stop T3
	timer:cancel(LoopDat#iac_state.t3),
	ToutPE = LoopDat#iac_state.t4_timeout_pe,
	case T4tout of
		ToutPE ->
			% set i to ie IAC->AERM
			send_to_aerm(set_i_to_ie, LoopDat);
		_ ->
			ok
	end,
	% send Start to AERM
	send_to_aerm(start, LoopDat),
	% start T4
	{ok, T4} = timer:apply_after(T4tout, gen_fsm, send_event,
				     [self(), {timer_expired, t4}]),
	% Cp := 0
	% cancel further proving?
	LoopDat2 = LoopDat#iac_state{t4 = T4, t4_timeout = T4tout,
				     cp = 0, further_prov = 0},
	{next_state, proving, LoopDat2};
aligned(emergency, LoopDat) ->
	% Send SIE
	send_to_txc(si_e, LoopDat),
	T4tout = LoopDat#iac_state.t4_timeout_pe,
	{next_State, aligned, LoopDat#iac_state{t4_timeout = T4tout}};
aligned(si_os, LoopDat) ->
	% Send alignment not possible
	send_to_lsc(alignment_not_possible, LoopDat),
	% stop T3
	timer:cancel(LoopDat#iac_state.t3),
	{next_state, idle, LoopDat#iac_state{emergency=0, t3=undefined}};
aligned(stop, LoopDat) ->
	% Stop T3
	timer:cancel(LoopDat#iac_state.t3),
	% cancel Emergency
	{next_state, idle, LoopDat#iac_state{emergency=0, t3=undefined}};
aligned({timer_expired, t3}, LoopDat) ->
	% Send alignment not possible
	send_to_lsc(alignment_not_possible, LoopDat),
	% cancel emergency
	{next_state, idle, LoopDat#iac_state{emergency=0}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE "proving"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fig9_4(LoopDat) ->
	% send Stop to AERM
	send_to_aerm(stop, LoopDat),
	% cancel emergency
	{next_state, idle, LoopDat#iac_state{emergency=0}}.

fig9_5(LoopDat) ->
	% send Start to AERM
	send_to_aerm(start, LoopDat),
	% cancel further proving
	% start T4
	T4tout = LoopDat#iac_state.t4_timeout,
	{ok, T4} = timer:apply_after(T4tout, gen_fsm, send_event,
				     [self(), {timer_expired, t4}]),
	{next_state, proving, LoopDat#iac_state{t4=T4, further_prov=0}}.

prov_emerg_or_sie(LoopDat) ->
	% stop T4
	timer:cancel(LoopDat#iac_state.t4),
	% Set T4 to Pe
	T4tout = LoopDat#iac_state.t4_timeout_pe,
	% Send stop to AERM
	send_to_aerm(stop, LoopDat),
	% Send 'set ti to tie' to AERM
	send_to_aerm(set_ti_to_tie, LoopDat),
	fig9_5(LoopDat#iac_state{t4_timeout=T4tout, t4=undefined}).


proving(expires, LoopDat) ->
	% alignment complete
	{next_state, idle, LoopDat};
proving(si_e, LoopDat) ->
	ToutPE = LoopDat#iac_state.t4_timeout_pe,
	case LoopDat#iac_state.t4_timeout of
		ToutPE ->
			{next_state, proving, LoopDat};
		_ ->
			prov_emerg_or_sie(LoopDat)
	end;
proving(emergency, LoopDat) ->
	prov_emerg_or_sie(LoopDat);
proving(stop, LoopDat) ->
	% stop T4
	timer:cancel(LoopDat#iac_state.t4),
	fig9_4(LoopDat);
proving(si_os, LoopDat) ->
	% stop T4
	timer:cancel(LoopDat#iac_state.t4),
	% Send alignment not possible to LSC
	send_to_lsc(alignment_not_possible, LoopDat),
	fig9_4(LoopDat);
proving(high_err_rate, LoopDat) ->
	% alignment not possible
	{next_state, idle, LoopDat};
proving(sio, LoopDat) ->
	% stop T4
	timer:cancel(LoopDat#iac_state.t4),
	% send Stop to AERM
	send_to_aerm(stop, LoopDat),
	% start T3
	T3tout = LoopDat#iac_state.t3_timeout,
	{ok, T3} = timer:apply_after(T3tout, gen_fsm, send_event,
				     [self(), {timer_expired, t3}]),
	{next_state, aligned, LoopDat#iac_state{t3=T3, t4=undefined}};
proving(What, LoopDat) when What == correct_su; What == si_n ->
	case LoopDat#iac_state.further_prov of
		1 ->
			% stop T4
			timer:cancel(LoopDat#iac_state.t4),
			fig9_5(LoopDat);
		_ ->
			{next_state, proving, LoopDat}
	end;
proving({timer_expired, t4}, LoopDat) ->
	% check if we are further proving, if yes, call fig9_5
	case LoopDat#iac_state.further_prov of
		1 ->
			fig9_5(LoopDat);
		_ ->
			% send 'aligment complete' to LSC
			send_to_lsc(alignment_complete, LoopDat),
			fig9_4(LoopDat)
	end;
proving(abort_proving, LoopDat) ->
	% Cp := Cp + 1
	Cp = LoopDat#iac_state.cp,
	LoopDat2 = LoopDat#iac_state{cp = Cp + 1},
	case Cp + 1 of
		5 ->
			% send 'alignment not possible' to LSC
			send_to_lsc(alignment_not_possible, LoopDat),
			% stop T4
			timer:cancel(LoopDat#iac_state.t4),
			fig9_4(LoopDat2);
		_ ->
			% mark further proving
			{next_state, proving, LoopDat2#iac_state{further_prov=1}}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_to_lsc(What, #iac_state{lsc_pid = Lsc}) ->
	gen_fsm:send_event(Lsc, What).

send_to_aerm(What, #iac_state{aerm_pid = Aerm}) ->
	Aerm ! {iac_aerm, What}.

send_to_txc(What, #iac_state{txc_pid = Txc}) ->
	Txc ! {iac_txc, What}.
