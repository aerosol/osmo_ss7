% SCTP wrapper behavior, used by M2PA/M2UA/M3UA/SUA 

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

-module(sctp_core).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(gen_fsm).

-include_lib("kernel/include/inet_sctp.hrl").
-include("osmo_util.hrl").

-export([start_link/1]).

-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

-export([behaviour_info/1]).

% FSM states:
-export([idle/2, associating/2, established/2]).

behaviour_info(callbacks) ->
	gen_fsm:behaviour_info(callbacks) ++ 
	[{rx_sctp, 4}, {mtp_xfer, 2}, {state_change, 2}, {prim_up, 3}];
behaviour_info(Other) ->
	gen_fsm:behaviour_info(Other).

% Loop Data
-record(sctp_state, {
	  role,		% passive | active
	  state,	% idle | associating | established
	  user_pid,
	  sctp_remote_ip,
	  sctp_remote_port,
	  sctp_local_port,
	  sctp_sock,
	  sctp_assoc_id,
	  module,	% callback module
	  ext_state	% state of the callback module
	}).

start_link(InitOpts) ->
	gen_fsm:start_link(?MODULE, InitOpts, [{debug, [trace]}]).

reconnect_sctp(L = #sctp_state{sctp_remote_ip = Ip, sctp_remote_port = Port, sctp_sock = Sock}) ->
	io:format("SCTP Reconnect ~p:~p~n", [Ip, Port]),
	timer:sleep(1*1000),
	InitMsg = #sctp_initmsg{num_ostreams = 2, max_instreams = 2},
	case gen_sctp:connect_init(Sock, Ip, Port, [{active, once}, {reuseaddr, true},
						    {sctp_initmsg, InitMsg}]) of
		ok ->
			ok;
		{error, Error } ->
			io:format("SCTP Error ~p, reconnecting~n", [Error]),
			reconnect_sctp(L)
	end.

init(InitOpts) ->
	OpenOptsBase = [{active, once}, {reuseaddr, true}],
	Module = proplists:get_value(module, InitOpts),
	ModuleArgs = proplists:get_value(module_args, InitOpts),
	LocalPort = proplists:get_value(sctp_local_port, InitOpts),
	Role = proplists:get_value(sctp_role, InitOpts),
	case LocalPort of
		undefined ->
			OpenOpts = OpenOptsBase;
		_ ->
			OpenOpts = OpenOptsBase ++ [{port, LocalPort}]
	end,
	{ok, SctpSock} = gen_sctp:open(OpenOpts),
	case Module:init(ModuleArgs) of
		{ok, ExtState} ->
			LoopDat = #sctp_state{role = Role, sctp_sock = SctpSock,
					user_pid = proplists:get_value(user_pid, InitOpts),
					ext_state = ExtState, module = Module,
					sctp_remote_ip = proplists:get_value(sctp_remote_ip, InitOpts),
					sctp_remote_port = proplists:get_value(sctp_remote_port, InitOpts),
					sctp_local_port = LocalPort},
			case Role of
				active ->
					gen_fsm:send_event(self(), osmo_util:make_prim('M','SCTP_ESTABLISH',request));
				_ ->
					ok
			end,
			{ok, idle, LoopDat};
		Default ->
			{error, {module_returned, Default}}
	end.

terminate(Reason, State, LoopDat) ->
	io:format("Terminating ~p (Reason: ~p)~n", [?MODULE, Reason]),
	Module = LoopDat#sctp_state.module,
	gen_sctp:close(LoopDat#sctp_state.sctp_sock),
	Module:terminate(Reason, State, LoopDat#sctp_state.ext_state).

code_change(OldVsn, StateName, LoopDat, Extra) ->
	Module = LoopDat#sctp_state.module,
	case Module:code_change(OldVsn, StateName, LoopDat#sctp_state.ext_state, Extra) of
		{ok, ExtState} ->
			{ok, StateName, LoopDat#sctp_state{ext_state = ExtState}};
		Other ->
			Other
	end.

% Helper function to send data to the SCTP peer
send_sctp_to_peer(LoopDat, PktData, StreamId, Ppid) when is_binary(PktData) ->
	#sctp_state{sctp_sock = Sock, sctp_assoc_id = Assoc} = LoopDat,
	SndRcvInfo = #sctp_sndrcvinfo{assoc_id = Assoc, ppid = Ppid, stream = StreamId},
	gen_sctp:send(Sock, SndRcvInfo, PktData).

send_prim_to_user(LoopDat, Prim) when is_record(LoopDat, sctp_state), is_record(Prim, primitive) ->
	%#m3ua_state{user_fun = Fun, user_args = Args} = LoopDat,
	%Fun(Prim, Args).
	UserPid = LoopDat#sctp_state.user_pid,
	UserPid ! Prim.

prim_up_to_callback(Prim, State, LoopDat) ->
	Module = LoopDat#sctp_state.module,
	case Module:prim_up(Prim, State, LoopDat#sctp_state.ext_state) of
		{ok, Prim, ExtNew} ->
			send_prim_to_user(LoopDat, Prim);
		{ignore, ExtNew} ->
			ok
	end,
	LoopDat#sctp_state{ext_state = ExtNew}.


handle_event(Event, State, LoopDat) ->
	Module = LoopDat#sctp_state.module,
	io:format("Unknown Event ~p in state ~p~n", [Event, State]),
	case Module:handle_event(Event, State, LoopDat#sctp_state.ext_state) of
		{next_state, State, ExtState} ->
			{next_state, State, LoopDat#sctp_state{ext_state = ExtState}}
	end.


handle_info({sctp, Socket, _RemoteIp, _RemotePort, {ANC, SAC}},
	     State, LoopDat) when is_record(SAC, sctp_assoc_change) ->
	io:format("SCTP Assoc Change ~p ~p~n", [ANC, SAC]),
	#sctp_assoc_change{state = SacState, outbound_streams = _OutStreams,
			   inbound_streams = _InStreams, assoc_id = AssocId} = SAC,
	if
		SacState == comm_up;
		SacState == restart ->
			case State of
				associating ->
					NewState = established,
					Spec = confirm;
				_ ->
					NewState = State,
					Spec = indication
			end,
			% primitive to the user
			LoopDat2 = prim_up_to_callback(osmo_util:make_prim('M','SCTP_ESTABLISH',Spec),
						       State, LoopDat);
		SacState == comm_lost ->
			case State of
				releasing ->
					Spec = confirm;
				_ ->
					Spec = indication
			end,
			LoopDat2 = prim_up_to_callback(osmo_util:make_prim('M','SCTP_RELEASE',Spec),
							State, LoopDat),
			case LoopDat#sctp_state.role of
				active ->
					NewState = associating,
					reconnect_sctp(LoopDat2);
				_ ->
					NewState = idle
			end;
		SacState == addr_unreachable;
		SacState == cant_assoc	->
			case LoopDat#sctp_state.role of
				active ->
					NewState = associating,
					reconnect_sctp(LoopDat);
				_ ->
					NewState = idle
			end,
			LoopDat2 = LoopDat
	end,
	inet:setopts(Socket, [{active, once}]),
	next_state(State, NewState, LoopDat2#sctp_state{sctp_assoc_id = AssocId});

handle_info({sctp, Socket, RemoteIp, RemotePort, {[Anc], Data}}, State, LoopDat) ->
	Module = LoopDat#sctp_state.module,
	io:format("SCTP rx data: ~p ~p~n", [Anc, Data]),
	% process incoming SCTP data
	if Socket == LoopDat#sctp_state.sctp_sock,
	   RemoteIp == LoopDat#sctp_state.sctp_remote_ip,
	   RemotePort == LoopDat#sctp_state.sctp_remote_port ->
		Ret = Module:rx_sctp(Anc, Data, State, LoopDat#sctp_state.ext_state),
		case Ret of
			{ok, Prim, ExtState} ->
				send_prim_to_user(LoopDat, Prim);
			{ignore, ExtState} ->
				ok
		end;
	   true ->
		io:format("unknown SCTP: ~p ~p~n", [Anc, Data]),
		ExtState = LoopDat#sctp_state.ext_state
	end,
	inet:setopts(Socket, [{active, once}]),
	next_state(State, State, LoopDat#sctp_state{ext_state = ExtState});

handle_info({sctp, Socket, RemoteIp, RemotePort, {_Anc, Data}}, State, LoopDat)
					when is_record(Data, sctp_shutdown_event) ->
	io:format("SCTP remote ~p:~p shutdown~n", [RemoteIp, RemotePort]),
	% FIXME: send SCTP_RELEASE.ind ?
	inet:setopts(Socket, [{active, once}]),
	case LoopDat#sctp_state.role of
		active ->
			reconnect_sctp(LoopDat);
		_ ->
			ok
	end,
	next_state(State, associating, LoopDat);

handle_info(Info, State, LoopDat) ->
	Module = LoopDat#sctp_state.module,
	case Module:handle_info(Info, State, LoopDat#sctp_state.ext_state) of
		{next_state, State, ExtState} ->
			{next_state, State, LoopDat#sctp_state{ext_state = ExtState}}
	end.


idle(#primitive{subsystem = 'M', gen_name = 'SCTP_ESTABLISH', spec_name = request}, LoopDat) ->
	% M-SCTP_ESTABLISH.req from User
	case LoopDat#sctp_state.role of
		active ->
			reconnect_sctp(LoopDat);
		_ ->
			ok
	end,
	next_state(idle, associating, LoopDat);
idle(Prim, LoopDat) when is_record(Prim, primitive) ->
	LoopDat2 = prim_up_to_callback(Prim, idle, LoopDat),
	next_state(idle, idle, LoopDat2).



associating(#primitive{subsystem = 'M', gen_name = 'SCTP_RELEASE',
			spec_name = request}, LoopDat) ->
	% M-SCTP_RELEASE.req from User
	% directly send RELEASE.conf ?!?
	next_state(associating, idle, LoopDat);
associating(Prim, LoopDat) when is_record(Prim, primitive) ->
	LoopDat2 = prim_up_to_callback(Prim, associating, LoopDat),
	next_state(associating, associating, LoopDat2).



established(#primitive{subsystem = 'M', gen_name = 'SCTP_RELEASE',
			spec_name = request}, LoopDat) ->
	% M-SCTP_RELEASE.req from User
	next_state(established, releasing, LoopDat);
established(#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		       spec_name = request, parameters = Params}, LoopDat) ->
	% MTP-TRANSFER.req from user app; Send message to remote peer
	Module = LoopDat#sctp_state.module,
	ExtState = Module:mtp_xfer(Params, LoopDat#sctp_state.ext_state),
	next_state(established, established, LoopDat#sctp_state{ext_state = ExtState});
established(#primitive{subsystem = 'SCTP', gen_name = 'TRANSFER',
		spec_name = request, parameters = {Stream, Ppid, Data}}, LoopDat) ->
	io:format("SCTP-TRANSFER.req~n",[]),
	% somebody (typically callback module) requests us to send SCTP data
	send_sctp_to_peer(LoopDat, Data, Stream, Ppid),
	next_state(established, established, LoopDat);
established(Prim, LoopDat) when is_record(Prim, primitive) ->
	LoopDat2 = prim_up_to_callback(Prim, established, LoopDat),
	next_state(established, established, LoopDat2).

next_state(State, NewState, LoopDat) when is_record(LoopDat, sctp_state) ->
	Module = LoopDat#sctp_state.module,
	case NewState of
		State ->
			{next_state, NewState, LoopDat};
		_ ->
			ExtState = Module:state_change(State, NewState, LoopDat#sctp_state.ext_state),
			{next_state, NewState, LoopDat#sctp_state{ext_state = ExtState}}
	end.
