% MGW Nat testing code

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

-module(osmo_ss7_pcap).
-author("Harald Welte <laforge@gnumonks.org>").
-export([pcap_apply/3]).

-define(NODEBUG, 1).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epcap/include/epcap_net.hrl").

-record(loop_data, {
		args,
		pkt_nr
	}).

pcap_apply(File, Filter, Args) ->
	epcap:start([{file, File}, {filter, Filter}]),
	loop(#loop_data{args = Args, pkt_nr = 1}).

loop(L = #loop_data{args=Args, pkt_nr = PktNr}) ->
	receive
		[{pkthdr, {_,_,_,{datalink,Datalink}}}, {packet, Packet}] ->
			Decaps = epcap_net:decapsulate_dlt(Datalink, Packet),
			handle_pkt_cb(PktNr, Decaps, Args),
			loop(L#loop_data{pkt_nr = PktNr+1});
		{epcap, eof} ->
			?debugFmt("EOF from PCAP~n", []),
			epcap:stop(),
			{ok, PktNr-1};
		Default ->
			?debugFmt("Unknown ~p from PCAP~n", [Default])
	end.

handle_pkt_cb(PktNr, [Ether, IP, Hdr, Payload], Args) ->
	?debugFmt("~p:~n  ~p/~p~n", [IP, Hdr, Payload]),
	case Hdr of
		#sctp{chunks = Chunks} ->
			Path = [{epcap_pkt_nr, PktNr}, Ether, IP, Hdr],
			handle_sctp_chunks(Chunks, Path, Args);
		_ ->
			ok
	end.

handle_sctp_chunks([], _Path, _Args) ->
	ok;
handle_sctp_chunks([Head|Tail], Path, Args) ->
	RewriteFn = proplists:get_value(rewrite_fn, Args),
	case Head of
		#sctp_chunk{type = 0, payload=#sctp_chunk_data{ppi=Ppi, data=Data}} ->
			%mgw_nat:mangle_rx_data(l, from_stp, Data, fun handle_rewrite_cb/5);
			put(rewrite_cb, RewriteFn),
			shim_rw_actor(sctp, from_msc, Path, Ppi, Data);
		_ ->
			ok
	end,
	handle_sctp_chunks(Tail, Path, Args).

shim_rw_actor(Proto, From, Path, MsgType, Msg) ->
	?debugFmt(" IN:~p:~p:~p~n", [Proto, From, Msg]),
	Fn = get(rewrite_cb),
	MsgOut = Fn(Proto, From, Path, MsgType, Msg),
	case MsgOut of
		Msg ->
			MsgOut;
		_ ->
			%io:format("OUT:~p:~p:~p~n", [Proto, From, MsgOut]),
			MsgOut
	end.
