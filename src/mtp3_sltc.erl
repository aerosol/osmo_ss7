% MTP3 Signalling Link Test Control (SLTC) according to Q.707

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

-module(mtp3_sltc).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(gen_fsm).

-include("mtp3.hrl").

% gen_fsm exports 
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

% individual FSM states
-export([idle/2, first_attempt/2, second_attempt/2]).

-record(sltc_state, {
		hmrt_pid,
		mgmt_pid,
		lsac_pid,
		sls,
		opc,
		adj_dpc,
		t1,
		t1_timeout,
		x
	}).

-define(SLTC_T1_DEF,	10000).
-define(SLTC_T2_DEF,	60000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Hmrt, Mgmt, Lsac, Sls, AdjDpc, Opc]) when
		is_pid(Hmrt), is_pid(Mgmt), is_pid(Lsac), is_integer(Sls) ->
	SltState = #sltc_state{hmrt_pid = Hmrt,
			       mgmt_pid = Mgmt,
			       lsac_pid = Lsac,
			       sls = Sls,
			       adj_dpc = AdjDpc,
			       opc = Opc,
			       t1_timeout = ?SLTC_T1_DEF,
			       x = 16#2342},
	{ok, idle, SltState}.

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

idle(M=#mtp3_msg{service_ind = ?MTP3_SERV_MGMT,
		 payload = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST,
			 		h1 = ?MTP3MG_H1_SLTM}}, LoopDat) ->
	Slta = slta_from_sltm(M),
	send_to(hmrt, Slta, LoopDat),
	{next_state, idle, LoopDat};

idle(start, LoopDat) ->
	Sltm = generate_sltm(LoopDat),
	send_to(hmrt, Sltm, LoopDat),
	{ok, T1} = timer:apply_after(gen_fsm, send_event,
				     [self(), {timer_expired, t1}]),
	{next_state, first_attempt, LoopDat#sltc_state{t1 = T1}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: first_attempt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

first_attempt(M=#mtp3_msg{service_ind = ?MTP3_SERV_MGMT,
			  payload = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST,
						h1 = ?MTP3MG_H1_SLTM}}, LoopDat) ->
	Slta = slta_from_sltm(M),
	send_to(hmrt, Slta, LoopDat),
	{next_state, first_attempt, LoopDat};

first_attempt(M = #mtp3_msg{service_ind = ?MTP3_SERV_MGMT,
			    payload = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST,
						  h1 = ?MTP3MG_H1_SLTA}}, LoopDat) ->
	timer:cancel(LoopDat#sltc_state.t1),
	case slt_matches(M, LoopDat) of
	    true ->
		send_to(lsac, slt_successful, LoopDat),
		{next_state, idle, LoopDat};
	    false ->
		Sltm = generate_sltm(LoopDat),
		send_to(hmrt, Sltm, LoopDat),
		{ok, T1} = timer:apply_after(gen_fsm, send_event,
					     [self(), {timer_expired, t1}]),
		{next_state, second_attempt, LoopDat#sltc_state{t1 = T1}}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STATE: second_attempt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

second_attempt(M=#mtp3_msg{service_ind = ?MTP3_SERV_MGMT,
			  payload = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST,
						h1 = ?MTP3MG_H1_SLTM}}, LoopDat) ->
	Slta = slta_from_sltm(M),
	send_to(hmrt, Slta, LoopDat),
	{next_state, second_attempt, LoopDat};

second_attempt(M = #mtp3_msg{service_ind = ?MTP3_SERV_MGMT,
			     payload = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST,
						   h1 = ?MTP3MG_H1_SLTA}}, LoopDat) ->
	timer:cancel(LoopDat#sltc_state.t1),
	case slt_matches(M, LoopDat) of
	    true ->
		send_to(lsac, slt_successful, LoopDat);
	    false ->
		send_to(mgmt, slt_failed, LoopDat),
		send_to(lsac, slt_failed, LoopDat)
	end,
	{next_state, idle, LoopDat};

second_attempt({timer_expired, t1}, LoopDat) ->
	send_to(mgmt, slt_failed, LoopDat),
	send_to(lsac, slt_failed, LoopDat),
	{next_state, idle, LoopDat}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_to(hmrt, What, #sltc_state{hmrt_pid = Txc}) ->
	Txc ! {sltc_hmrt, What};
send_to(mgmt, What, #sltc_state{mgmt_pid = Txc}) ->
	Txc ! {sltc_mgmt, What};
send_to(lsac, What, #sltc_state{lsac_pid = Txc}) ->
	Txc ! {sltc_lsac, What}.

slta_from_sltm(M = #mtp3_msg{service_ind = ?MTP3_SERV_MGMT,
			     routing_label = RoutLbl,
			     payload = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST,
						   h1 = ?MTP3MG_H1_SLTM,
					   	   test_pattern = TP}}) ->
	InvRoutLbl = invert_rout_lbl(RoutLbl),
	M#mtp3_msg{routing_label = InvRoutLbl,
		   payload = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST,
			   		 h1 = ?MTP3MG_H1_SLTA,
					 test_pattern = TP}}.

generate_sltm(LoopDat) ->
	Mg = #mtp3mg_msg{h0 = ?MTP3MG_H0_TEST, h1 = ?MTP3MG_H1_SLTM,
			 test_pattern = LoopDat#sltc_state.x},
	Lbl = #mtp3_routing_label{sig_link_sel = LoopDat#sltc_state.sls,
				  origin_pc = LoopDat#sltc_state.opc,
				  dest_pc = LoopDat#sltc_state.adj_dpc},

	#mtp3_msg{network_ind = ?MTP3_NETIND_INTERNATIONAL,
		  service_ind = ?MTP3_SERV_MGMT,
		  routing_label = Lbl, payload = Mg}.

rout_lbl_matches(#mtp3_routing_label{sig_link_sel = SlsLocal,
				     origin_pc = OPC}, LoopDat) ->
	#sltc_state{adj_dpc = AdjDpc, sls = SLS} = LoopDat,
	if SLS == SlsLocal, OPC == AdjDpc ->
		true;
	   true ->
		false
	end.

slt_matches(#mtp3_msg{routing_label = RoutLbl,
		      payload = #mtp3mg_msg{test_pattern = TP}}, LoopDat) ->
	case LoopDat#sltc_state.x of
	    TP ->
		rout_lbl_matches(RoutLbl, LoopDat);
	    _ ->
		false
	end.



invert_rout_lbl(L = #mtp3_routing_label{origin_pc = Opc, dest_pc = Dpc}) ->
	L#mtp3_routing_label{origin_pc = Dpc, dest_pc = Opc}.
