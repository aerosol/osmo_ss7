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
		next_local_ref,
		user_pid,	% pid() of the user process
		mtp_tx_action	% action to be performed for MTP-TRANSFER.req
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   API                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(InitData) ->
	% make sure to store the Pid of the caller in the scrc_state
	gen_fsm:start_link(sccp_scrc, [{user_pid,self()}|InitData], [{debug, [trace]}]).

init(InitPropList) ->
	io:format("SCRC Init PropList~p ~n", [InitPropList]),
	UserPid = proplists:get_value(user_pid, InitPropList),
	MtpTxAct = proplists:get_value(mtp_tx_action, InitPropList),
	LoopData = #scrc_state{user_pid = UserPid, mtp_tx_action = MtpTxAct, next_local_ref = 0},
	{ok, idle, LoopData}.


terminate(Reason, _State, _LoopDat) ->
	io:format("SCRC: Terminating with reason ~p~n", [Reason]),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          FSM callbacks (states)                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% N-UNITDATA.req from user (normally this is SCLC, but we don't have SCLC)
idle(#primitive{subsystem = 'N', gen_name = 'UNITDATA',
		   spec_name = request, parameters = Params}, LoopDat) ->
	% User needs to specify: Protocol Class, Called Party, Calling Party, Data
	% FIXME: implement XUDT / LUDT support
	% encode the actual SCCP message
        io:format("------------------------~n"),
        io:format("~p~n", [Params]),
        io:format("------------------------~n"),
        CingP = proplists:get_value(calling_party_addr, Params),
        CedP = proplists:get_value(called_party_addr, Params),
        OPC = CedP#sccp_addr.point_code,
        DPC = CingP#sccp_addr.point_code,
        io:format("DPC ~p~n", [DPC]),
        case scrc_gtt:needs_translation(CedP) of
            true ->
                TranslatedPC = 101; %% TODO

            %[{protocol_class,{1,0}},
             %{called_party_addr,
                 %{sccp_addr,undefined,undefined,undefined,undefined,
                     %{global_title,1,0,undefined,undefined,undefined,"48111222333"}}},
             %{calling_party_addr,{sccp_addr,undefined,undefined,101,123,undefined}},
             %{user_data,<<"message">>}]

            false ->
                TranslatedPC = DPC
        end,
	EncMsg = sccp_codec:encode_sccp_msgt(?SCCP_MSGT_UDT, Params),
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg, OPC, TranslatedPC),
	{next_state, idle, LoopDat}.

%idle(#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		%spec_name = indication, parameters = Params}, LoopDat) ->
	%{ok, Msg} = sccp_codec:parse_sccp_msg(Params#mtp3_msg.payload),
	%io:format("Parsed Msg: ~p LoopDat ~p ~n", [Msg, LoopDat]),
	%case Msg of
		%% special handling for CR message here in SCRC
		%#sccp_msg{msg_type = ?SCCP_MSGT_CR} ->
                    %io:format("This should be here - we're connection less now! ~p~n", [Msg]);
		%%#sccp_msg{msg_type = ?SCCP_MSGT_IT} ->
		%_ ->
			%IsConnLess = sccp_codec:is_connectionless(Msg),
			%case IsConnLess of
				%true ->
                                    %UserPid = LoopDat#scrc_state.user_pid,
                                    %% FIXME: N-NOTICE.ind for NOTICE
                                    %UserPrim = osmo_util:make_prim('N','UNITDATA', indication, Msg),
                                    %UserPid ! {sccp, UserPrim};
                                %_Oops ->
                                    %io:format("This should be here - we're connection less now! ~p~n", [Msg])
			%end
	%end,
	%{next_state, idle, LoopDat};

%idle(sclc_scrc_connless_msg, LoopDat) ->
	%% FIXME: get to MTP-TRANSFER.req
	%{next_state, idle, LoopDat}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Layer communication                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% execute user callback function with MTP3 message
%%
send_mtp_down(#scrc_state{mtp_tx_action = MtpTxAction}, Prim) ->
	io:format("MTP Tx ~p, Prim ~p~n", [MtpTxAction, Prim]),
	case MtpTxAction of
		{callback_fn, Function, Args} ->
			Function(Prim, Args);
		_ ->
			{error, "Unknown MtpTxAction"}
	end.

%% encode MTP3 message, pass it to the lower layer within proper primitive
%%
send_mtp_transfer_down(LoopDat, EncMsg, OPC, DPC) ->
        %% TODO routing label destination point code
	Rlbl = #mtp3_routing_label{sig_link_sel = 0, origin_pc = OPC, dest_pc = DPC},
	Mtp3 = #mtp3_msg{network_ind = ?MTP3_NETIND_INTERNATIONAL,
			 service_ind = ?MTP3_SERV_SCCP,
			 routing_label = Rlbl, payload = EncMsg},
	MtpPrim = #primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
			     spec_name = request, parameters = Mtp3},
	send_mtp_down(LoopDat, MtpPrim).
