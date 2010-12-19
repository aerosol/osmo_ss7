
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

-module(sccp_user).
-author('Harald Welte <laforge@gnumonks.org>').
-export([init/3]).

-include("sccp.hrl").

-define(IPA_STREAM_ID_SCCP, 253).

-record(loop_data, {
		ipa_stream_id
	}).

init(TcpServerPort, IpaStreamId, Opts) ->
	ipa_proto:init(),
	% Create listening IPA socket
	ipa_proto:start_listen(TcpServerPort, 1, Opts),
	loop(#loop_data{ipa_stream_id = IpaStreamId}).

% callback function to be called by IPA socket handler if it receives some data
sccp_ipa_adapter_cb(S, IpaStreamID, DataBin, [ScrcPid]) ->
	io:format("sccp_ipa_adapter_cb (Socket ~p, Stream ~p), passing data to SCRP~n", [S, IpaStreamID]),
	% hand any incoming IPA message off into the SCCP stacks SCRC
	gen_fsm:send_event(ScrcPid, sccp_scoc:make_prim('MTP', 'TRANSFER', indication, DataBin)).

% callback function to be called by SCCP if it wants to transmit some data
sccp_to_ipa_cb(#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
			  spec_name = request, parameters = DataBin}, [IpaPid, S, IpaStreamID]) ->
	%ipa_proto:send(S, IpaStreamID, DataBin).
	io:format("sccp_to_ipa_cb: Sending to ~p ~p/~p: ~p~n", [IpaPid, S,IpaStreamID, DataBin]),
	IpaPid ! {ipa_send, S, IpaStreamID, DataBin}.

loop(LoopData) ->
	receive
		{ipa_tcp_accept, S} ->
			io:format("sccp_ipa_adapter: ipa_tcp_accept from ~p~n", [inet:peername(S)]),
			% hand over the socket into the IPA stack
			{ok, IpaPid} = ipa_proto:register_socket(S),
			% Start the SCRC FSM for this virtual MTP link
			ScrcMtpCb = {callback_fn, fun sccp_to_ipa_cb/2, [IpaPid, S, ?IPA_STREAM_ID_SCCP]},
			{ok, ScrcPid} = sccp_scrc:start_link([{mtp_tx_action, ScrcMtpCb}]),
			% Register an IPA stream for SCCP
			ipa_proto:register_stream(S, ?IPA_STREAM_ID_SCCP,
						  {callback_fn, fun sccp_ipa_adapter_cb/4, [ScrcPid]}),
			ipa_proto:unblock(S),
			loop(LoopData);
		% this code should later be moved into the actual MSC
		{sccp, Prim} ->
			io:format("sccp_user has received primitive ~p~n", [Prim]),
			handle_sccp_prim(Prim),
			loop(LoopData)
	end.


handle_sccp_prim(#primitive{subsystem = 'N', gen_name = 'CONNECT',
			    spec_name = indication, parameters = Params}) ->
	%RespPrim = Prim#primitive{spec_name = response},
	RespPrim = sccp_scoc:make_prim('N', 'CONNECT', response, []),
	ScocPid = proplists:get_value(scoc_pid, Params),
	gen_fsm:send_event(ScocPid, RespPrim);
handle_sccp_prim(#primitive{}) ->
	ok.
