% MTP2 Link State Control according to Q.703 Figure 3 / Figure 8

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

-module(mtp2_lsc).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(gen_fsm).

% gen_fsm exports 
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

% individual FSM states
-export([power_off/2, out_of_service/2, initial_alignment/2,
	 aligned_not_ready/2, aligned_ready/2, in_service/2,
	 processor_outage/2]).

% sync event handlers
-export([power_off/3]).

-record(lsc_state, {
		t1_timeout,
		t1,
		iac_pid,
		aerm_pid,
		l3_pid,
		poc_pid,
		txc_pid,
		local_proc_out,
		proc_out,
		emergency
	}).

-define(M2PA_T1_DEF,	300000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Aerm, Txc, L3, Poc]) ->
	{ok, Iac} = gen_fsm:start_link(mtp2_iac, [self(), Aerm, Txc], [{debug, [trace]}]),
	LscState = #lsc_state{t1_timeout = ?M2PA_T1_DEF,
			      iac_pid = Iac,
			      aerm_pid = Aerm,
			      l3_pid = L3,
			      poc_pid = L3,
			      txc_pid = Txc,
		      	      local_proc_out = 0,
		      	      proc_out = 0,
		      	      emergency = 0},
	{ok, power_off, LscState}.

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
% STATE: power_off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

power_off(power_on, LoopDat) ->
	% Power On from MGMT
	send_to(txc, start, LoopDat),
	send_to(txc, si_os, LoopDat),
	send_to(aerm, set_ti_to_tin, LoopDat),
	% Cancel local processor outage, cancel emergency
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0, emergency=0}}.

power_off(get_iac_pid, From, LoopDat) ->
	Iac = LoopDat#lsc_state.iac_pid,
	{reply, {ok, Iac}, power_off, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: out_of_service
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

out_of_service(start, LoopDat) ->
	% Start from L3
	send_to(rc, start, LoopDat),
	send_to(txc, start, LoopDat),
	case LoopDat#lsc_state.emergency of
		1 ->
			send_to(iac, emergency, LoopDat);
		_ ->
			ok
	end,
	send_to(iac, start, LoopDat),
	{next_state, initial_alignment, LoopDat};

out_of_service(retrieve_bsnt, LoopDat) ->
	send_to(rc, retrieve_bsnt, LoopDat),
	{next_state, out_of_service, LoopDat};

out_of_service(retrieval_request_and_fsnc, LoopDat) ->
	send_to(txc, retrieval_request_and_fsnc, LoopDat),
	{next_state, out_of_service, LoopDat};

out_of_service(emergency, LoopDat) ->
	{next_state, out_of_service, LoopDat#lsc_state{emergency=1}};

out_of_service(emergency_ceases, LoopDat) ->
	{next_state, out_of_service, LoopDat#lsc_state{emergency=0}};

out_of_service(What, LoopDat) when	What == local_processor_outage;
					What == level3_failure ->
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=1}};

out_of_service(si_os, LoopDat) ->
	% this transition is not specified in Q.703, but it makes
	% quite a bit of sense.  yate M2PA requires it, too.
	{next_state, out_of_service, LoopDat};

out_of_service(local_processor_recovered, LoopDat) ->
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: initial_alignment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_alignment(What, LoopDat) when	What == local_processor_outage;
					What == level3_failure ->
	{next_state, initial_alignment, LoopDat#lsc_state{local_proc_out=1}};

initial_alignment(local_processor_recovered, LoopDat) ->
	{next_state, initial_alignment, LoopDat#lsc_state{local_proc_out=0}};

initial_alignment(emergency, LoopDat) ->
	send_to(iac, emergency, LoopDat),
	{next_state, initial_alignment, LoopDat#lsc_state{emergency=1}};

initial_alignment(alignment_complete, LoopDat) ->
	send_to(suerm, start, LoopDat),
	{ok, T1} = timer:apply_after(LoopDat#lsc_state.t1_timeout,
				     gen_fsm, send_event,
				     [self(), {timer_expired, t1}]),
	case LoopDat#lsc_state.local_proc_out of
		1 ->
			send_to(poc, local_processor_outage, LoopDat),
			send_to(txc, si_po, LoopDat),
			send_to(rc, reject_msu_fiso, LoopDat),
			NextState = aligned_not_ready;
		_ ->
			send_to(txc, fisu, LoopDat),
			send_to(rc, accept_msu_fiso, LoopDat),
			NextState = aligned_ready
	end,
	{next_state, NextState, LoopDat#lsc_state{t1=T1}};

initial_alignment(stop, LoopDat) ->
	send_to(iac, stop, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0, emergency=0}};

initial_alignment(link_failure, LoopDat) ->
	send_to(l3, out_of_service, LoopDat),
	send_to(iac, stop, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0, emergency=0}};

initial_alignment(alignment_not_possible, LoopDat) ->
	send_to(rc, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0, emergency=0}};

% forward into IAC sub-state-machine
initial_alignment(What, LoopDat) when
		What == si_n; What == si_e; What == si_o; What == si_os;
		What == fisu_msu_received ->
	Iac = LoopDat#lsc_state.iac_pid,
	gen_fsm:send_event(Iac, What),
	{next_state, initial_alignment, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: aligned_ready
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aligned_ready(SioOrSios, LoopDat) when SioOrSios == si_o;
					SioOrSios == si_os;
			       		SioOrSios == link_failure ->
	timer:cancel(LoopDat#lsc_state.t1),
	send_to(l3, out_of_service, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat};

aligned_ready(stop, LoopDat) ->
	timer:cancel(LoopDat#lsc_state.t1),
	send_to(rc, stop, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat};

aligned_ready({timer_expired, t1}, LoopDat) ->
	send_to(l3, out_of_service, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat};

aligned_ready(si_po, LoopDat) ->
	timer:cancel(LoopDat#lsc_state.t1),
	send_to(l3, remote_processor_outage, LoopDat),
	send_to(poc, remote_processor_outage, LoopDat),
	{next_state, processor_outage, LoopDat};

aligned_ready(fisu_msu_received, LoopDat) ->
	send_to(l3, in_service, LoopDat),
	timer:cancel(LoopDat#lsc_state.t1),
	send_to(txc, msu, LoopDat),
	{next_state, in_service, LoopDat};
aligned_ready(What, LoopDat) when	What == local_processor_outage;
					What == level3_failure ->
	send_to(poc, local_processor_outage, LoopDat),
	send_to(txc, si_po, LoopDat),
	send_to(rc, reject_msu_fiso, LoopDat),
	{next_state, aligned_not_ready, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: aligned_not_ready
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aligned_not_ready(Err, LoopDat) when 	Err == link_failure;
					Err == si_o;
					Err == si_os ->
	timer:cancel(LoopDat#lsc_state.t1),
	send_to(l3, out_of_service, LoopDat),
	send_to(l3, stop, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	send_to(poc, stop, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0, emergency=0}};

aligned_not_ready(stop, LoopDat) ->
	timer:cancel(LoopDat#lsc_state.t1),
	send_to(l3, stop, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	send_to(poc, stop, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0, emergency=0}};

aligned_not_ready({timer_expired, t1}, LoopDat) ->
	send_to(l3, stop, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	send_to(poc, stop, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{local_proc_out=0, emergency=0}};

aligned_not_ready(local_processor_recovered, LoopDat) ->
	send_to(poc, local_processor_recovered, LoopDat),
	send_to(txc, fisu, LoopDat),
	send_to(rc, accept_msu_fisu, LoopDat),
	{next_state, aligned_ready, LoopDat#lsc_state{local_proc_out=0}};

aligned_not_ready(fisu_msu_received, LoopDat) ->
	send_to(l3, in_service, LoopDat),
	timer:cancel(LoopDat#lsc_state.t1),
	{next_state, processor_outage, LoopDat};

aligned_not_ready(si_po, LoopDat) ->
	send_to(l3, remote_processor_outage, LoopDat),
	send_to(poc, remote_processor_outage, LoopDat),
	timer:cancel(LoopDat#lsc_state.t1),
	{next_state, processor_outage, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: in_service
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_service(What, LoopDat) when	What == link_failure;
				What == si_o;
				What == si_n;
				What == si_e;
				What == si_os ->
	send_to(l3, out_of_service, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{emergency=0}};

in_service(stop, LoopDat) ->
	send_to(suerm, stop, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{emergency=0}};

in_service(What, LoopDat) when	What == local_processor_outage;
				What == level3_failure ->
	send_to(poc, local_processor_outage, LoopDat),
	send_to(txc, si_po, LoopDat),
	send_to(rc, reject_msu_fisu, LoopDat),
	{next_state, aligned_not_ready, LoopDat#lsc_state{local_proc_out=1}};

in_service(si_po, LoopDat) ->
	send_to(txc, fisu, LoopDat),
	send_to(l3, remote_processor_outage, LoopDat),
	send_to(poc, remote_processor_outage, LoopDat),
	{next_state, processor_outage, LoopDat#lsc_state{proc_out=1}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: processor_outage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processor_outage(retrieval_request_and_fsnc, LoopDat) ->
	send_to(txc, retrieval_request_and_fsnc, LoopDat),
	{next_state, processor_outage, LoopDat};

processor_outage(fisu_msu_received, LoopDat) ->
	send_to(poc, remote_processor_recovered, LoopDat),
	send_to(l3, remote_processor_recovered, LoopDat),
	{next_state, processor_outage, LoopDat};

processor_outage(retrieve_bsnt, LoopDat) ->
	send_to(rc, retrieve_bsnt, LoopDat),
	{next_state, processor_outage, LoopDat};

processor_outage(What, LoopDat) when	What == local_processor_outage;
					What == level3_failure ->
	send_to(poc, local_processor_outage, LoopDat),
	send_to(txc, si_po, LoopDat),
	{next_state, processor_outage, LoopDat#lsc_state{local_proc_out=1}};

processor_outage(si_po, LoopDat) ->
	send_to(l3, remote_processor_outage, LoopDat),
	send_to(poc, remote_processor_outage, LoopDat),
	{next_state, processor_outage, LoopDat#lsc_state{proc_out=1}};

processor_outage(local_processor_recovered, LoopDat) ->
	send_to(poc, local_processor_recovered, LoopDat),
	send_to(rc, retrieve_fsnx, LoopDat),
	send_to(txc, fisu, LoopDat),
	{next_state, processor_outage, LoopDat};

processor_outage(flush_buffers, LoopDat) ->
	send_to(txc, flush_buffers, LoopDat),
	% FIXME: mark L3 ind recv
	{next_state, processor_outage, LoopDat};

processor_outage(no_processor_outage, LoopDat) ->
	% FIXME: check L3 ind
	send_to(txc, msu, LoopDat),
	send_to(rc, accept_msu_fisu, LoopDat),
	{next_state, in_service, LoopDat#lsc_state{local_proc_out=0, proc_out=0}};

processor_outage(What, LoopDat) when	What == link_failure;
					What == si_o;
					What == si_n;
					What == si_e;
					What == si_os ->
	send_to(l3, out_of_service, LoopDat),
	send_to(suerm, stop, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(poc, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{emergency=0, local_proc_out=0}};

processor_outage(stop, LoopDat) ->
	send_to(suerm, stop, LoopDat),
	send_to(rc, stop, LoopDat),
	send_to(poc, stop, LoopDat),
	send_to(txc, si_os, LoopDat),
	{next_state, out_of_service, LoopDat#lsc_state{emergency=0, local_proc_out=0}}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_to(txc, What, #lsc_state{txc_pid = Txc}) ->
	Txc ! {lsc_txc, What};
send_to(iac, What, #lsc_state{iac_pid = Iac}) ->
	gen_fsm:send_event(Iac, What);
send_to(Who, What, _LoopDat) ->
	io:format("Not sending LSC -> ~p: ~p~n", [Who, What]).

