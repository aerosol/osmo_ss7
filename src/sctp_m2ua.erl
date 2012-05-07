% M2UA in accordance with RFC3331 (http://tools.ietf.org/html/rfc3331)

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

-module(sctp_m2ua).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(sctp_core).

-include_lib("kernel/include/inet_sctp.hrl").
-include("osmo_util.hrl").
-include("m2ua.hrl").

-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

-export([rx_sctp/4, mtp_xfer/2, state_change/3, prim_up/3]).

-record(m2ua_state, {
		asp_pid,
		last_bsn_received,
		last_fsn_sent
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_InitOpts) ->
	{ok, Asp} = gen_fsm:start_link(xua_asp_fsm, [sua_asp, [], Fun, [self()], self()], [{debug, [trace]}]),
	{ok, #m2ua_state{last_bsn_received=16#ffffff, last_fsn_sent=16#ffffff, asp_pid=Asp}}

terminate(Reason, _State, _LoopDat) ->
	io:format("Terminating ~p (Reason ~p)~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, _State, LoopDat, _Extra) ->
	{ok, LoopDat}.

handle_event(_Event, State, LoopDat) ->
	{next_state, State, LoopDat}.

handle_info(_Info, State, LoopDat) ->
	{next_state, State, LoopDat}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sctp_core callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_up(#primitive{subsystem='M', gen_name = 'SCTP_ESTABLISH', spec_name = confirm}, State, LoopDat) ->
	Asp = LoopDat#m2ua_state.asp_pid,
	gen_fsm:send_event(Asp, osmo_util:make_prim('M','ASP_UP',request)),
	{ignore, LoopDat};
prim_up(#primitive{subsystem='M', gen_name = 'ASP_UP', spec_name = confirm}, State, LoopDat) ->
	Asp = LoopDat#m2ua_state.asp_pid,
	gen_fsm:send_event(Asp, osmo_util:make_prim('M','ASP_ACTIVE',request)),
	{ignore, LoopDat};
prim_up(Prim, State, LoopDat) ->
	% default: forward all primitives to the user 
	{ok, Prim, LoopDat}.


% sctp_core indicates that we have received some data...
rx_sctp(#sctp_sndrcvinfo{ppid = ?M2UA_PPID}, Data, State, LoopDat) ->
	Asp = LoopDat#m2ua_state.asp_pid,
	{ok, M2ua} = xua_codec:parse_msg(Data),
	% FIXME: check sequenc number linearity
	case M2ua of
		#xua_msg{msg_class = ?M3UA_MSGC_SSNM} ->
			% FIXME
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M3UA_MSGC_ASPSM} ->
			gen_fsm:send_event(Asp, M2ua),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M3UA_MSGC_ASPTM} ->
			gen_fsm:send_event(Asp, M2ua),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M2UA_CLASS_M2UA,
			  msg_type = ?M2UA_TYPE_USER} ->
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
		_ ->
			% do something with link related msgs
			io:format("M2UA Unknown message ~p in state ~p~n", [M2pa, State]),
			{ignore, State, LoopDat}
	end.

% MTP-TRANSFER.req has arrived at sctp_core, encapsulate+tx it
mtp_xfer(Mtp3, LoopDat) ->
	Fsn = inc_seq_nr(LoopDat#m2pa_state.last_fsn_sent),
	M2ua = #xua_msg{msg_class = ?M2UA_CLASS_M2UA,
			 msg_type = ?M2UA_TYPE_USER,
			 mtp3 = Mtp3},
	M2paBin = xua_codec:encode_msg(M2ua),
	tx_sctp(?M2UA_STREAM_USER, M2paBin),
	LoopDat2.

state_change(_, established, LoopDat) ->
	% emulate a 'start' from LSC
	%gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, start),
	LoopDat;
state_change(established, _, LoopDat) ->
	%gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, link_failure),
	LoopDat;
state_change(_, _, LoopDat) ->
	LoopDat.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tx_sctp(Stream, Payload) when is_integer(Stream), is_binary(Payload) ->
	Param = {Stream, ?M2UA_PPID, Payload},
	% sent to 'ourselves' (behaviour master module)
	gen_fsm:send_event(self(), osmo_util:make_prim('SCTP','TRANSFER',request,Param)).
