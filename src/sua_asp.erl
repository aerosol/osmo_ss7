% SCCP SUA ASP xua_asp_fsm callback according to RFC3868 4.3.1

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

-module(sua_asp).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(xua_asp_fsm).

-include("osmo_util.hrl").
-include("m3ua.hrl").
-include("sua.hrl").

-export([init/1]).

-export([gen_xua_msg/3, asp_down/3, asp_inactive/3, asp_active/3]).

init([]) ->
	{ok, we_have_no_state}.

gen_xua_msg(MsgClass, MsgType, Params) ->
	#sua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType, payload = Params}.

asp_down(#sua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType},
	 LoopDat, Mld) when MsgClass == ?M3UA_MSGC_ASPSM; MsgClass == ?M3UA_MSGC_ASPTM ->
	% convert from SUA to xua_msg and call into master module
	xua_asp_fsm:asp_down({xua_msg, MsgClass, MsgType}, Mld);
asp_down(SuaMsg, LoopDat, Mld) when is_record(SuaMsg, sua_msg) ->
	rx_sua(SuaMsg, asp_down, Mld).

asp_inactive(#sua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType},
	     LoopDat, Mld) when MsgClass == ?M3UA_MSGC_ASPSM; MsgClass == ?M3UA_MSGC_ASPTM ->
	% convert from SUA to xua_msg and call into master module
	xua_asp_fsm:asp_inactive({xua_msg, MsgClass, MsgType}, Mld);
asp_inactive(SuaMsg, LoopDat, Mld) when is_record(SuaMsg, sua_msg) ->
	rx_sua(SuaMsg, asp_inactive, Mld).

asp_active(#sua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType},
	   LoopDat, Mld) when MsgClass == ?M3UA_MSGC_ASPSM; MsgClass == ?M3UA_MSGC_ASPTM ->
	% convert from SUA to xua_msg and call into master module
	xua_asp_fsm:asp_active({xua_msg, MsgClass, MsgType}, Mld);
asp_active(SuaMsg, LoopDat, Mld) when is_record(SuaMsg, sua_msg) ->
	rx_sua(SuaMsg, asp_active, Mld).



rx_sua(Msg = #sua_msg{version = 1, msg_class = ?M3UA_MSGC_ASPSM,
		      msg_type = ?M3UA_MSGT_ASPSM_BEAT}, State, LoopDat) ->
	% Send BEAT_ACK using the same payload as the BEAT msg
	xua_asp_fsm:send_sctp_to_peer(LoopDat, Msg#sua_msg{msg_type = ?M3UA_MSGT_ASPSM_BEAT_ACK}),
	{next_state, State, LoopDat};

%rx_sua(Msg = #sua_msg{version = 1, msg_class = ?M3UA_MSGC_SSNM,
		       %msg_type = MsgType, payload = Params}, State, LoopDat) ->
	% transform to classic MTP primitive and send up to the user
	%Mtp = map_ssnm_to_mtp_prim(MsgType),
	%send_prim_to_user(LoopDat, Mtp),
	%{next_state, State, LoopDat};

rx_sua(Msg = #sua_msg{}, State, LoopDat) ->
	io:format("SUA Unknown messge ~p in state ~p~n", [Msg, State]),
	{next_state, State, LoopDat}.
