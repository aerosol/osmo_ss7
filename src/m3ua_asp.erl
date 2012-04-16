% M3UA ASP xua_asp_fsm callback

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

-module(m3ua_asp).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(xua_asp_fsm).

-include("osmo_util.hrl").
-include("m3ua.hrl").

-export([init/1]).

-export([gen_xua_msg/3, asp_down/3, asp_inactive/3, asp_active/3]).

init([]) ->
	{ok, we_have_no_state}.

gen_xua_msg(MsgClass, MsgType, Params) ->
	#m3ua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType, payload = Params}.

asp_down(#m3ua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType},
	 LoopDat, Mld) when MsgClass == ?M3UA_MSGC_ASPSM; MsgClass == ?M3UA_MSGC_ASPTM ->
	% convert from M3UA to xua_msg and call into master module
	xua_asp_fsm:asp_down({xua_msg, MsgClass, MsgType}, Mld);
asp_down(M3uaMsg, LoopDat, Mld) when is_record(M3uaMsg, m3ua_msg) ->
	rx_m3ua(M3uaMsg, asp_down, Mld).

asp_inactive(#m3ua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType},
	     LoopDat, Mld) when MsgClass == ?M3UA_MSGC_ASPSM; MsgClass == ?M3UA_MSGC_ASPTM ->
	% convert from M3UA to xua_msg and call into master module
	xua_asp_fsm:asp_inactive({xua_msg, MsgClass, MsgType}, Mld);
asp_inactive(M3uaMsg, LoopDat, Mld) when is_record(M3uaMsg, m3ua_msg) ->
	rx_m3ua(M3uaMsg, asp_inactive, Mld).

asp_active(#m3ua_msg{version = 1, msg_class = MsgClass, msg_type = MsgType},
	   LoopDat, Mld) when MsgClass == ?M3UA_MSGC_ASPSM; MsgClass == ?M3UA_MSGC_ASPTM ->
	% convert from M3UA to xua_msg and call into master module
	xua_asp_fsm:asp_active({xua_msg, MsgClass, MsgType}, Mld);
asp_active(M3uaMsg, LoopDat, Mld) when is_record(M3uaMsg, m3ua_msg) ->
	rx_m3ua(M3uaMsg, asp_active, Mld).





rx_m3ua(Msg = #m3ua_msg{version = 1, msg_class = ?M3UA_MSGC_MGMT,
			msg_type = ?M3UA_MSGT_MGMT_NTFY}, State, LoopDat) ->
	xua_asp_fsm:send_prim_to_user(LoopDat, osmo_util:make_prim('M','NOTIFY',indication,[Msg])),
	{next_state, State, LoopDat};

rx_m3ua(Msg = #m3ua_msg{version = 1, msg_class = ?M3UA_MSGC_ASPSM,
			msg_type = ?M3UA_MSGT_ASPSM_BEAT}, State, LoopDat) ->
	% Send BEAT_ACK using the same payload as the BEAT msg
	xua_asp_fsm:send_sctp_to_peer(LoopDat, Msg#m3ua_msg{msg_type = ?M3UA_MSGT_ASPSM_BEAT_ACK}),
	{next_state, State, LoopDat};

rx_m3ua(Msg = #m3ua_msg{version = 1, msg_class = ?M3UA_MSGC_MGMT,
			msg_type = ?M3UA_MSGT_MGMT_ERR}, State, LoopDat) ->
	xua_asp_fsm:send_prim_to_user(LoopDat, osmo_util:make_prim('M','ERROR',indication,[Msg])),
	{next_state, State, LoopDat};

rx_m3ua(Msg = #m3ua_msg{version = 1, msg_class = ?M3UA_MSGC_SSNM,
		        msg_type = MsgType, payload = Params}, State, LoopDat) ->
	% transform to classic MTP primitive and send up to the user
	Mtp = map_ssnm_to_mtp_prim(MsgType),
	xua_asp_fsm:send_prim_to_user(LoopDat, Mtp),
	{next_state, State, LoopDat};

rx_m3ua(Msg = #m3ua_msg{}, State, LoopDat) ->
	io:format("M3UA Unknown messge ~p in state ~p~n", [Msg, State]),
	{next_state, State, LoopDat}.

% Transform the M3UA SSNM messages into classic MTP primitives
map_ssnm_to_mtp_prim(MsgType) ->
	Mtp = #primitive{subsystem = 'MTP', spec_name = indication},
	case MsgType of
	    ?M3UA_MSGT_SSNM_DUNA -> Mtp#primitive{gen_name = 'PAUSE'};
	    ?M3UA_MSGT_SSNM_DAVA -> Mtp#primitive{gen_name = 'RESUME'};
	    ?M3UA_MSGT_SSNM_SCON -> Mtp#primitive{gen_name = 'STATUS'};
	    ?M3UA_MSGT_SSNM_DUPU -> Mtp#primitive{gen_name = 'STATUS'}
	end.
