% M2PA in accordance with RFC4165 (http://tools.ietf.org/html/rfc4665)

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

-module(sctp_m2pa).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(sctp_core).

-include_lib("kernel/include/inet_sctp.hrl").
-include("osmo_util.hrl").
-include("m2pa.hrl").

-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

-export([rx_sctp/4, mtp_xfer/2, state_change/3, prim_up/3]).

-record(m2pa_state, {
		last_bsn_received,
		last_fsn_sent,
		lsc_pid,
		iac_pid,
		msu_fisu_accepted
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_InitOpts) ->
	% start MTP2 IAC FSM pointing LSC, AERM and TXC to us
	{ok, Lsc} = gen_fsm:start_link(mtp2_lsc, [self(), self(), self(), self(),self()], [{debug, [trace]}]),
	{ok, Iac} = gen_fsm:sync_send_event(Lsc, get_iac_pid),
	gen_fsm:send_event(Lsc, power_on),
	{ok, #m2pa_state{last_bsn_received=16#ffffff, last_fsn_sent=16#ffffff,
			 lsc_pid=Lsc, iac_pid=Iac,
		 	 msu_fisu_accepted = 0}}.

terminate(Reason, _State, _LoopDat) ->
	io:format("Terminating ~p (Reason ~p)~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, _State, LoopDat, _Extra) ->
	{ok, LoopDat}.

handle_event(_Event, State, LoopDat) ->
	{next_state, State, LoopDat}.

handle_info({lsc_txc, What}, State, LoopDat) when
			What == start; What == retrieval_request_and_fsnc ->
	{next_state, State, LoopDat};
handle_info({lsc_rc, accept_msu_fisu}, State, LoopDat) ->
	{next_state, State, LoopDat#m2pa_state{msu_fisu_accepted = 1}};
handle_info({lsc_rc, reject_msu_fisu}, State, LoopDat) ->
	{next_state, State, LoopDat#m2pa_state{msu_fisu_accepted = 0}};
handle_info({Who, What}, established, LoopDat) when Who == iac_txc; Who == lsc_txc ->
	Ls = iac_to_ls(What),
	send_linkstate(Ls, LoopDat),
	{next_state, established, LoopDat};
handle_info(_Info, State, LoopDat) ->
	{next_state, State, LoopDat}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sctp_core callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_up(Prim, State, LoopDat) ->
	% default: forward all primitives to the user 
	{ok, Prim, LoopDat}.


% sctp_core indicates that we have received some data...
rx_sctp(#sctp_sndrcvinfo{ppid = ?M2PA_PPID}, Data, State, LoopDat) ->
	{ok, M2pa} = m2pa_codec:parse_msg(Data),
	FsnRecv = M2pa#m2pa_msg.fwd_seq_nr,
	% FIXME: check sequenc number linearity
	case M2pa of
		#m2pa_msg{msg_class = ?M2PA_CLASS_M2PA,
			  msg_type = ?M2PA_TYPE_USER} ->
			Mtp3 = M2pa#m2pa_msg.mtp3,
			case LoopDat#m2pa_state.msu_fisu_accepted of
				1 ->
					LoopDat2 = LoopDat#m2pa_state{last_bsn_received = FsnRecv},
					case Mtp3 of
						undefined ->
							ok;
						_ ->
							send_userdata_ack(LoopDat2)
					end,
					gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, fisu_msu_received),
					Prim = osmo_util:make_prim('MTP','TRANSFER',indication, Mtp3),
					{ok, Prim, LoopDat2};
				_ ->
					{ignore, LoopDat}
			end;
		#m2pa_msg{msg_type = ?M2PA_TYPE_LINK} ->
			handle_linkstate(M2pa, LoopDat),
			{ignore, LoopDat};
		_ ->
			% do something with link related msgs
			io:format("M2PA Unknown message ~p in state ~p~n", [M2pa, State]),
			{ignore, State, LoopDat}
	end.

% MTP-TRANSFER.req has arrived at sctp_core, encapsulate+tx it
mtp_xfer(Mtp3, LoopDat) ->
	Fsn = inc_seq_nr(LoopDat#m2pa_state.last_fsn_sent),
	M2pa = #m2pa_msg{msg_class = ?M2PA_CLASS_M2PA,
			 msg_type = ?M2PA_TYPE_USER,
			 fwd_seq_nr = Fsn,
			 back_seq_nr = LoopDat#m2pa_state.last_bsn_received,
			 mtp3 = Mtp3},
	M2paBin = m2pa_codec:encode_msg(M2pa),
	LoopDat2 = LoopDat#m2pa_state{last_fsn_sent = Fsn},
	tx_sctp(?M2PA_STREAM_USER, M2paBin),
	LoopDat2.

state_change(_, established, LoopDat) ->
	% emulate a 'start' from LSC
	gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, start),
	LoopDat;
state_change(established, _, LoopDat) ->
	gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, link_failure),
	LoopDat;
state_change(_, _, LoopDat) ->
	LoopDat.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inc_seq_nr(SeqNr) when is_integer(SeqNr) ->
	SeqNr + 1 rem 16#FFFFFF.

handle_linkstate(M2pa, LoopDat) when is_record(M2pa, m2pa_msg) ->
	Linkstate = proplists:get_value(link_state, M2pa#m2pa_msg.parameters),
	LsMtp2 = ls_to_iac(Linkstate),
	if LsMtp2 == fisu ->
		case LoopDat#m2pa_state.msu_fisu_accepted of
			1 ->
				gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid,
						   fisu_msu_received);
			0 -> ok
		end;
	   LsMtp2 == si_po ->
		gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, LsMtp2);
	   LsMtp2 == si_n; LsMtp2 == si_e; LsMtp2 == si_o; LsMtp2 == si_os ->
		gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, LsMtp2)
		% IAC will receive the event as pass-through from LSC
		%gen_fsm:send_event(LoopDat#m2pa_state.iac_pid, LsMtp2)
	end.

% convert M2PA link state to MTP2
ls_to_iac(?M2PA_LS_OOS) ->
	si_os;
ls_to_iac(?M2PA_LS_ALIGNMENT) ->
	si_o;
ls_to_iac(?M2PA_LS_PROVING_NORMAL) ->
	si_n;
ls_to_iac(?M2PA_LS_PROVING_EMERG) ->
	si_e;
ls_to_iac(?M2PA_LS_READY) ->
	fisu;
ls_to_iac(?M2PA_LS_PROC_OUTAGE) ->
	si_po;
ls_to_iac(?M2PA_LS_PROC_RECOVERED) ->
	fisu;
ls_to_iac(?M2PA_LS_BUSY) ->
	si_b.
% FIXME: what about BUSY_ENDED?


% convert MTP2 link state to M2PA
iac_to_ls(si_os) ->
	?M2PA_LS_OOS;
iac_to_ls(si_o) ->
	?M2PA_LS_ALIGNMENT;
iac_to_ls(si_n) ->
	?M2PA_LS_PROVING_NORMAL;
iac_to_ls(si_e) ->
	?M2PA_LS_PROVING_EMERG;
iac_to_ls(fisu) ->
	?M2PA_LS_READY;
iac_to_ls(msu) ->
	?M2PA_LS_READY;
iac_to_ls(si_po) ->
	?M2PA_LS_PROC_OUTAGE;
iac_to_ls(si_b) ->
	?M2PA_LS_BUSY.

% Chapter 4.1.2 of RFC4165
ls_stream(?M2PA_LS_PROC_OUTAGE) ->
	1;
ls_stream(?M2PA_LS_PROC_RECOVERED) ->
	1;
ls_stream(Foo) when is_integer(Foo) ->
	0.

send_linkstate(Ls, LoopDat) when is_integer(Ls) ->
	Stream = ls_stream(Ls),
	M2pa = #m2pa_msg{msg_class = ?M2PA_CLASS_M2PA,
			 msg_type = ?M2PA_TYPE_LINK,
			 fwd_seq_nr = LoopDat#m2pa_state.last_fsn_sent,
			 back_seq_nr = LoopDat#m2pa_state.last_bsn_received,
			 parameters = [{link_state, Ls}]},
	M2paBin = m2pa_codec:encode_msg(M2pa),
	tx_sctp(Stream, M2paBin),
	LoopDat.

send_userdata_ack(LoopDat) ->
	M2pa = #m2pa_msg{msg_class = ?M2PA_CLASS_M2PA,
			 msg_type = ?M2PA_TYPE_USER,
			 fwd_seq_nr = LoopDat#m2pa_state.last_fsn_sent,
			 back_seq_nr = LoopDat#m2pa_state.last_bsn_received},
	M2paBin = m2pa_codec:encode_msg(M2pa),
	tx_sctp(0, M2paBin).

tx_sctp(Stream, Payload) when is_integer(Stream), is_binary(Payload) ->
	Param = {Stream, ?M2PA_PPID, Payload},
	% sent to 'ourselves' (behaviour master module)
	gen_fsm:send_event(self(), osmo_util:make_prim('SCTP','TRANSFER',request,Param)).
