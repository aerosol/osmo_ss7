% SUA behaviour call-back for sctp_core

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

-module(sctp_sua).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(sctp_core).

-include_lib("kernel/include/inet_sctp.hrl").
-include("osmo_util.hrl").
-include("xua.hrl").
-include("sua.hrl").
-include("m3ua.hrl").

-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

-export([rx_sctp/4, mtp_xfer/2, state_change/3, prim_up/3]).

-record(sua_state, {
		asp_pid
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_InitOpts) ->
	% start SUA ASP
	Fun = fun(Prim, Args) -> asp_prim_to_user(Prim, Args) end,
	{ok, Asp} = gen_fsm:start_link(xua_asp_fsm, [sua_asp, [], Fun, [self()], self()], [{debug, [trace]}]),
	{ok, #sua_state{asp_pid=Asp}}.

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
	Asp = LoopDat#sua_state.asp_pid,
	gen_fsm:send_event(Asp, osmo_util:make_prim('M','ASP_UP',request)),
	{ignore, LoopDat};
prim_up(#primitive{subsystem='M', gen_name = 'ASP_UP', spec_name = confirm}, State, LoopDat) ->
	Asp = LoopDat#sua_state.asp_pid,
	gen_fsm:send_event(Asp, osmo_util:make_prim('M','ASP_ACTIVE',request)),
	{ignore, LoopDat};
prim_up(Prim, State, LoopDat) ->
	% default: forward all primitives to the user 
	{ok, Prim, LoopDat}.


% sctp_core indicates that ew have received some data...
rx_sctp(#sctp_sndrcvinfo{ppid = ?SUA_PPID}, Data, State, LoopDat) ->
	Asp = LoopDat#sua_state.asp_pid,
	Sua = xua_codec:parse_msg(Data),
	case Sua of
		#xua_msg{msg_class = ?M3UA_MSGC_MGMT,
			 msg_type = ?M3UA_MSGT_MGMT_NTFY} ->
			Prim = osmo_util:make_prim('M','NOTIFY',indication,Sua),
			{ok, Prim, LoopDat};
		#xua_msg{msg_class = ?M3UA_MSGC_MGMT,
			 msg_type = ?M3UA_MSGT_MGMT_ERR} ->
			Prim = osmo_util:make_prim('M','ERROR',indication,Sua),
			{ok, Prim, LoopDat};
		#xua_msg{msg_class = ?M3UA_MSGC_SSNM} ->
			% FIXME
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M3UA_MSGC_ASPSM} ->
			gen_fsm:send_event(Asp, Sua),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M3UA_MSGC_ASPTM} ->
			gen_fsm:send_event(Asp, Sua),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?SUA_MSGC_CL} ->
			Prim = sua_to_prim(Sua, LoopDat),
			{ok, Prim, LoopDat};
		%#xua_msg{msg_class = ?SUA_MSGC_C0} ->
		_ ->
			% do something with link related msgs
			io:format("SUA Unknown message ~p in state ~p~n", [Sua, State]),
			{ignore, State, LoopDat}
	end.

% MTP-TRANSFER.req has arrived at sctp_core, encapsulate+tx it
mtp_xfer(Sua, LoopDat) when is_record(Sua, xua_msg) ->
	SuaBin = xua_codec:encode_msg(Sua),
	tx_sctp(1, SuaBin),
	LoopDat.

state_change(_, established, LoopDat) ->
	% emulate a 'start' from LSC
	%gen_fsm:send_event(LoopDat#sua_state.lsc_pid, start),
	LoopDat;
state_change(established, _, LoopDat) ->
	%gen_fsm:send_event(LoopDat#sua_state.lsc_pid, link_failure),
	LoopDat;
state_change(_, _, LoopDat) ->
	LoopDat.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tx_sctp(Stream, Payload) when is_integer(Stream), is_binary(Payload) ->
	Param = {Stream, ?SUA_PPID, Payload},
	% sent to 'ourselves' (behaviour master module)
	gen_fsm:send_event(self(), osmo_util:make_prim('SCTP','TRANSFER',request,Param)).

% callback fun for ASP FMS
asp_prim_to_user(Prim, [SctpPid]) ->
	gen_fsm:send_event(SctpPid, Prim).


sua_to_prim(Sua, LoopDat) when is_record(Sua, xua_msg) ->
	Sccp = sua_sccp_conv:sua_to_sccp(Sua),
	osmo_util:make_prim('N','UNITADATA',indication, Sccp).
