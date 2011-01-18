% SCTP Handler for gateway between MSC and STP, transparently
% rewriting addresses on the fly

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
% (C) 2011 OnWaves
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


-module(sctp_handler).
-author("Harald Welte <laforge@gnumonks.org>").
-export([init/5]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

-include("m2ua.hrl").
-include("mtp3.hrl").
-include("isup.hrl").
-include("sccp.hrl").

-record(loop_data,
	{msc_sock, msc_local_ip, msc_remote_ip, msc_remote_port,
	 msc_local_port, msc_assoc_id, 
	 stp_sock, stp_remote_ip, stp_remote_port, stp_assoc_id
	}).

-define(COMMON_SOCKOPTS, [{active, once}, {reuseaddr, true}]).

% initialize the sockets towards MSC (listening) and STP (connect)
init(MscLocalIP, MscLocalPort, MscRemoteIP, StpRemoteIP, StpRemotePort) ->
	{ok, MscSock} = gen_sctp:open([{ip, MscLocalIP},{port,MscLocalPort}]
					++ ?COMMON_SOCKOPTS),
	io:format("Listening for MSC on ~w:~w. ~w~n",
			[MscLocalIP, MscLocalPort, MscSock]),
	ok = gen_sctp:listen(MscSock, true),
	{ok, StpSock} = gen_sctp:open(?COMMON_SOCKOPTS),
	L = #loop_data{msc_sock = MscSock, msc_local_ip = MscLocalIP, 
			msc_remote_ip = MscRemoteIP,
			stp_sock = StpSock, stp_remote_ip = StpRemoteIP,
			stp_remote_port = StpRemotePort},
	loop(L).

% initiate a connection to STP as a client
initiate_stp_connection(#loop_data{stp_sock = Sock, stp_remote_ip = IP, stp_remote_port = Port}, Opts) ->
	io:format("Establishing SCTP conn to STP ~p Port ~p~n", [IP, Port]),
	gen_sctp:connect(Sock, IP, Port, Opts ++ ?COMMON_SOCKOPTS).

% main loop function
loop(L = #loop_data{msc_sock=MscSock, msc_remote_ip=MscRemoteIp, msc_remote_port=MscRemotePort,
		    stp_sock=StpSock, stp_remote_ip=StpRemoteIp, stp_remote_port=StpRemotePort}) ->
	io:format("Entering receive loop ~p~n", [L]),
	io:format("======================================================================~n"),
	receive
		% MSC connect or disconnect
		{sctp, MscSock, MscRemoteIp, Port, {ANC, SAC}}
					when is_record(SAC, sctp_assoc_change) ->
			io:format("MSC sctp_assoc_change ~p ~p~n", [ANC, SAC]),
			#sctp_assoc_change{state = SacState, outbound_streams = OutStreams,
					   inbound_streams = InStreams, assoc_id = MscAssocId} = SAC,
			case SacState of
				comm_up ->
					InitMsg = #sctp_initmsg{num_ostreams=InStreams,
								max_instreams=OutStreams},
					{ok, StpAssoc} = initiate_stp_connection(L, [{sctp_initmsg,InitMsg}]),
					io:format("STP Assoc: ~p~n", [StpAssoc]),
					NewL = L#loop_data{msc_remote_port = Port,
						 msc_assoc_id = MscAssocId,
						 stp_assoc_id = StpAssoc#sctp_assoc_change.assoc_id};
				comm_lost ->
					NewL = L,
					% maybe we should simply die?
					foo:bar();
				addr_unreachable ->
					NewL = L,
					% maybe we should simply die?
					foo:bar()
			end,
			inet:setopts(MscSock, [{active, once}]);
		% STP connect or disconnect
		{sctp, StpSock, StpRemoteIp, StpRemotePort, {_Anc, SAC}}
					when is_record(SAC, sctp_assoc_change) ->
			io:format("STP sctp_assoc_change ~p~n", [SAC]),
			inet:setopts(StpSock, [{active, once}]),
			NewL = L;
		% MSC data
		{sctp, MscSock, MscRemoteIp, MscRemotePort, {[Anc], Data}} ->
			io:format("MSC rx data: ~p ~p~n", [Anc, Data]),
			handle_rx_data(L, from_msc, Anc, Data),
			inet:setopts(MscSock, [{active, once}]),
			NewL = L;
		% STP data
		{sctp, StpSock, StpRemoteIp, StpRemotePort, {[Anc], Data}} ->
			io:format("STP rx data: ~p ~p~n", [Anc, Data]),
			handle_rx_data(L, from_stp, Anc, Data),
			inet:setopts(StpSock, [{active, once}]),
			NewL = L;
		Other ->
			io:format("OTHER ~p~n", [Other]),
			NewL = L
	end,
	loop(NewL).


% handle incoming data on one of the SCTP sockets
handle_rx_data(L, From, SRInf = #sctp_sndrcvinfo{ppid = 2, 
						 stream = Stream}, Data) when is_binary(Data) ->
	DataOut = mangle_rx_data(L, From, Data),
	% send mangled data to other peer
	case From of
		from_msc ->
			Sock = L#loop_data.stp_sock,
			AssocId = L#loop_data.stp_assoc_id;
		from_stp ->
			Sock = L#loop_data.msc_sock,
			AssocId = L#loop_data.msc_assoc_id
	end,
	SndRcvInfo = #sctp_sndrcvinfo{ppid = 2, stream = Stream, assoc_id = AssocId},
	io:format("Sending ~p to ~p ~p~n", [DataOut, Sock, SndRcvInfo]),
	% if they are not equal, we will abort here
	DataOut = Data,
	io:format("Data is equal~n"),
	ok = gen_sctp:send(Sock, SndRcvInfo, DataOut).

% mangle the received data
mangle_rx_data(L, From, Data) when is_binary(Data) ->
	{ok, M2ua} = m2ua_codec:parse_m2ua_msg(Data),
	io:format("M2UA Decode: ~p~n", [M2ua]),
	case M2ua of
		#m2ua_msg{msg_class = ?M2UA_MSGC_MAUP,
			  msg_type = ?M2UA_MAUP_MSGT_DATA} ->
			M2ua_out = mangle_rx_m2ua_maup(L, From, M2ua);
		#m2ua_msg{} ->
			% simply pass it along unmodified
			M2ua_out = M2ua
	end,
	% re-encode the data
	io:format("M2UA Encode: ~p~n", [M2ua_out]),
	m2ua_codec:encode_m2ua_msg(M2ua_out).

% mangle the received M2UA
mangle_rx_m2ua_maup(L, From, M2ua = #m2ua_msg{parameters = Params}) ->
	{_Len, M2uaPayload} = proplists:get_value(16#300, Params),
	Mtp3 = mtp3_codec:parse_mtp3_msg(M2uaPayload),
	io:format("MTP3 Decode: ~p~n", [Mtp3]),
	Mtp3_out = mangle_rx_mtp3(L, From, Mtp3),
	io:format("MTP3 Encode: ~p~n", [Mtp3_out]),
	Mtp3OutBin = mtp3_codec:encode_mtp3_msg(Mtp3_out),
	Params2 = proplists:delete(16#300, Params),
	ParamsNew = Params2 ++ [{16#300, {byte_size(Mtp3OutBin), Mtp3OutBin}}],
	% return mangled parsed m2ua msg
	M2ua#m2ua_msg{parameters = ParamsNew}.

% mangle the MTP3 payload
mangle_rx_mtp3(L, From, Mtp3 = #mtp3_msg{service_ind = Service}) ->
	mangle_rx_mtp3_serv(L, From, Service, Mtp3).

% mangle the ISUP content
mangle_rx_mtp3_serv(L, From, ?MTP3_SERV_ISUP, Mtp3 = #mtp3_msg{payload = Payload}) ->
	io:format("ISUP~n"),
	% FIXME
	%Isup = parse_isup_msg(Payload),
	Mtp3;
% mangle the SCCP content
mangle_rx_mtp3_serv(L, From, ?MTP3_SERV_SCCP, Mtp3 = #mtp3_msg{payload = Payload}) ->
	Sccp = sccp_codec:parse_sccp_msg(Payload),
	io:format("SCCP Decode: ~p~n", [Sccp]),
	% FIXME
	Mtp3;
% default: do nothing
mangle_rx_mtp3_serv(_L, _From, _, Mtp3) ->
	Mtp3.
