% SCCP routing control procedures (SCRC)

% (C) 2010-2011 by Harald Welte <laforge@gnumonks.org>
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
-export([start_link/1, init/1, terminate/3, idle/2]).

-include("osmo_util.hrl").
-include("sccp.hrl").
-include("mtp3.hrl").



-record(scrc_state, {
		scoc_conn_ets,
		next_local_ref,
		user_pid,	% pid() of the user process
		mtp_tx_action	% action to be performed for MTP-TRANSFER.req
	}).
% TODO: 

tx_prim_to_local_ref(Prim, LocalRef) ->
	% determine the Pid to which the primitive must be sent
	ConnTable = get(scoc_by_ref),
	case ets:lookup(ConnTable, LocalRef) of
		[{LocalRef, ScocPid}] ->
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


terminate(Reason, _State, _LoopDat) ->
	io:format("SCRC: Terminating with reason ~p~n", [Reason]),
	Tref = get(scoc_by_ref),
	ets:delete(Tref),
	ok.

% helper function to create new SCOC instance
spawn_new_scoc(LoopDat) ->
	% create new SCOC instance
	UserPid = LoopDat#scrc_state.user_pid,
	% Compute the new local reference
	LocalRef = LoopDat#scrc_state.next_local_ref + 1,
	LoopDat1 = LoopDat#scrc_state{next_local_ref = LocalRef},
	% generate proplist for SCRC initialization
	ScocPropList = [{scrc_pid, self()}, {user_pid, UserPid}, {local_reference, LocalRef}],
	{ok, ScocPid} = sccp_scoc:start_link(ScocPropList),
	% insert SCOC instance in connection table
	ConnTable = get(scoc_by_ref),
	ets:insert_new(ConnTable, {LocalRef, ScocPid}),
	{LoopDat1, ScocPid}.


% N-CONNECT.req from user: spawn new SCOC and deliver primitive to it
idle(P = #primitive{subsystem = 'N', gen_name = 'CONNECT',
		    spec_name = request, parameters = Params}, LoopDat) ->
	% Start new SCOC instance
	{LoopDat1, ScocPid} = spawn_new_scoc(LoopDat),
	% Deliver primitive to new SCOC instance
	gen_fsm:send_event(ScocPid, P),
	{next_state, idle, LoopDat1};

% N-UNITDATA.req from user (normally this is SCLC, but we don't have SCLC)
idle(P= #primitive{subsystem = 'N', gen_name = 'UNITDATA',
		   spec_name = request, parameters = Params}, LoopDat) ->
	% User needs to specify: Protocol Class, Called Party, Calling Party, Data
	% FIXME: implement XUDT / LUDT support
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msgt(?SCCP_MSGT_UDT, Params),
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg),
	{next_state, idle, LoopDat};

idle(#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		spec_name = indication, parameters = Params}, LoopDat) ->
	{ok, Msg} = sccp_codec:parse_sccp_msg(Params#mtp3_msg.payload),
	io:format("Parsed Msg: ~p LoopDat ~p ~n", [Msg, LoopDat]),
	case Msg of
		% special handling for CR message here in SCRC
		#sccp_msg{msg_type = ?SCCP_MSGT_CR} ->
			{LoopDat1, ScocPid} = spawn_new_scoc(LoopDat),
			% send a RCOC-CONNECTING.ind primitive to the new SCOC fsm
			UserPrim = sccp_scoc:make_prim('RCOC','CONNECTION', indication, Msg#sccp_msg.parameters),
			io:format("Sending ~p to ~p~n", [UserPrim, ScocPid]),
			gen_fsm:send_event(ScocPid, UserPrim);
		% T(ias) expired on the other end of the connection
		%#sccp_msg{msg_type = ?SCCP_MSGT_IT} ->
		_ ->
			IsConnLess = sccp_codec:is_connectionless(Msg),
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
					ScocPrim = sccp_scoc:make_prim('RCOC', 'CONNECTION-MSG', indication, Msg),
					case LocalRef of
						undefined ->
							% FIXME: send SCCP_MSGT_ERR
							io:format("Conn-Msg to undefined ref ~p~n", [Msg]);
						_ ->
							tx_prim_to_local_ref(ScocPrim, LocalRef)
					end
			end,
			LoopDat1 = LoopDat
	end,
	{next_state, idle, LoopDat1};
idle(sclc_scrc_connless_msg, LoopDat) ->
	% FIXME: get to MTP-TRANSFER.req
	{next_state, idle, LoopDat};
% connection oriented messages like N-DATA.req from user
idle(#primitive{subsystem = 'OCRC', gen_name = 'CONNECTION-MSG',
		spec_name = request, parameters = Msg}, LoopDat) ->
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msg(Msg),
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg),
	{next_state, idle, LoopDat};
% SCOC has received confirmation about new incoming connection from user
idle(#primitive{subsystem = 'OCRC', gen_name = 'CONNECTION',
		spec_name = confirm, parameters = Params}, LoopDat) ->
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msgt(?SCCP_MSGT_CC, Params),
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg),
	{next_state, idle, LoopDat};


% triggered by N-CONNECT.req from user to SCOC:
idle(#primitive{subsystem = 'OCRC', gen_name = 'CONNECTION',
		spec_name = indication, parameters = Params}, LoopDat) ->
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msgt(?SCCP_MSGT_CR, Params),
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg),
	{next_state, idle, LoopDat}.

send_mtp_down(#scrc_state{mtp_tx_action = MtpTxAction}, Prim) ->
	io:format("MTP Tx ~p, Prim ~p~n", [MtpTxAction, Prim]),
	case MtpTxAction of
		{callback_fn, Function, Args} ->
			Function(Prim, Args);
		_ ->
			{error, "Unknown MtpTxAction"}
	end.

send_mtp_transfer_down(LoopDat, EncMsg) ->
	Rlbl = #mtp3_routing_label{sig_link_sel = 0, origin_pc = 123, dest_pc = 456},
	Mtp3 = #mtp3_msg{network_ind = ?MTP3_NETIND_INTERNATIONAL,
			 service_ind = ?MTP3_SERV_SCCP,
			 routing_label = Rlbl, payload = EncMsg},
	MtpPrim = #primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
			     spec_name = request, parameters = Mtp3},
	send_mtp_down(LoopDat, MtpPrim).
