% SCCP routing control procedures (SCRC)

% (C) 2010 by Harald Welte <laforge@gnumonks.org>
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

-module(sccp_scrc).
-behaviour(gen_fsm).
-export([start_link/1, init/1, idle/2]).

-include("sccp.hrl").



-record(scrc_state, {
		scoc_conn_ets,
		next_local_ref,
		user_pid,	% pid() of the user process
		mtp_tx_action	% action to be performed for MTP-TRANSFER.req
	}).
% TODO: 

% is the supplied message type a connectionless message?
is_connectionless(MsgType) ->
	case MsgType of
		?SCCP_MSGT_UDT -> true;
		?SCCP_MSGT_UDTS -> true;
		?SCCP_MSGT_XUDT -> true;
		?SCCP_MSGT_XUDTS -> true;
		?SCCP_MSGT_LUDT -> true;
		?SCCP_MSGT_LUDTS -> true;
		_ -> false
	end.

tx_prim_to_local_ref(Prim, LocalRef) ->
	% determine the Pid to which the primitive must be sent
	ConnTable = get(scoc_by_ref),
	case ets:lookup(ConnTable, LocalRef) of
		{ok, ScocPid} ->	
			gen_fsm:send_event(ScocPid, Prim);
		_ ->
			io:format("Primitive ~p for unknown local reference ~p~n",
				  [Prim, LocalRef])
	end.


start_link(InitData) ->
	% make sure to store the Pid of the caller in the scrc_state
	gen_fsm:start_link(sccp_scrc, [{user_pid,self()}|InitData], [{debug, [trace]}]).

init(InitPropList) ->
	io:format("SCRC Init PropList~p ~n", [InitPropList]),
	UserPid = proplists:get_value(user_pid, InitPropList),
	MtpTxAct = proplists:get_value(mtp_tx_action, InitPropList),
	LoopData = #scrc_state{user_pid = UserPid, mtp_tx_action = MtpTxAct, next_local_ref = 0},
	TableRef = ets:new(scoc_by_ref, [set]),
	put(scoc_by_ref, TableRef),
	{ok, idle, LoopData}.


idle(Prim = #primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		       spec_name = indication, parameters = Params}, LoopDat) ->
	{ok, Msg} = sccp_codec:parse_sccp_msg(Params),
	io:format("Parsed Msg: ~p LoopDat ~p ~n", [Msg, LoopDat]),
	case Msg of
		% special handling for CR message here in SCRC
		#sccp_msg{msg_type = ?SCCP_MSGT_CR} ->
			% create new SCOC instance
			UserPid = LoopDat#scrc_state.user_pid,
			% Compute the new local reference
			LocalRef = LoopDat#scrc_state.next_local_ref + 1,
			LoopDat1 = LoopDat#scrc_state{next_local_ref = LocalRef},
			% generate proplist for SCRC initialization
			ScocPropList = [{scrc_pid, self()}, {user_pid, UserPid}, {local_reference, LocalRef}],
			{ok, ScocPid} = sccp_scoc:start_link(ScocPropList),
			% send a RCOC-CONNECTING.ind primitive to the new SCOC fsm
			UserPrim = sccp_scoc:make_prim('RCOC','CONNECTION', indication, Msg#sccp_msg.parameters),
			io:format("Sending ~p to ~p~n", [UserPrim, ScocPid]),
			gen_fsm:send_event(ScocPid, UserPrim);
		_ ->
			
			IsConnLess = is_connectionless(Msg#sccp_msg.msg_type),
			case IsConnLess of
				true ->
					% it would be more proper to send them via SCLC ??
					%gen_fsm:send(sccp_sclc, ??
					UserPid = LoopDat#scrc_state.user_pid,
					% FIXME: N-NOTICE.ind for NOTICE 
					UserPrim = sccp_scoc:make_prim('N','UNITDATA', indication, Msg),
					UserPid ! {sccp, UserPrim};
				false ->
					% connection oriented messages need to go via SCOC instance
					#sccp_msg{parameters = Opts} = Msg,
					LocalRef = proplists:get_value(dst_local_ref, Opts),
					case LocalRef of
						undefined ->
							% FIXME: send SCCP_MSGT_ERR
							io:format("Conn-Msg to undefined ref ~p~n", [Msg]);
						_ ->
							tx_prim_to_local_ref(Prim, LocalRef)
					end
			end,
			LoopDat1 = LoopDat
	end,
	{next_state, idle, LoopDat1};
idle(sclc_scrc_connless_msg, LoopDat) ->
	% FIXME: get to MTP-TRANSFER.req
	{next_state, idle, LoopDat};
idle(sclc_scrc_conn_msg, LoopDat) ->
	{next_state, idle, LoopDat};
% SCOC has received confirmation about new incoming connection from user
idle(Prim = #primitive{subsystem = 'OCRC', gen_name = 'CONNECTION',
		       spec_name = confirm, parameters = Params}, LoopDat) ->
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msgt(?SCCP_MSGT_CC, Params),
	% generate a MTP-TRANSFER.req primitive to the lower layer
	MtpPrim = #primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
			     spec_name = request, parameters = EncMsg},
	send_mtp_down(LoopDat, MtpPrim),
	{next_state, idle, LoopDat};


% triggered by N-CONNECT.req from user to SCOC:
idle(Prim = #primitive{subsystem = 'OCRC', gen_name = 'CONNECTION',
			spec_name = indication, parameters = Params}, LoopDat) ->
	{next_state, idle, LoopDat}.


send_mtp_down(#scrc_state{mtp_tx_action = MtpTxAction}, Prim) ->
	io:format("MTP Tx ~p, Prim ~p~n", [MtpTxAction, Prim]),
	case MtpTxAction of
		{callback_fn, Function, Args} ->
			Function(Prim, Args);
		_ ->
			{error, "Unknown MtpTxAction"}
	end.
