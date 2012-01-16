% RFC 4165 MTP2 P2P Adaption Layer coding / decoding

% (C) 2012 by Harald Welte <laforge@gnumonks.org>
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

-module(m2pa_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("m2pa.hrl").
-include("mtp3.hrl").

-export([parse_msg/1, encode_msg/1]).

-compile({parse_transform, exprecs}).
-export_records([m2pa_msg]).

parse_m2pa_msgt(?M2PA_CLASS_M2PA, ?M2PA_TYPE_LINK, Len, Remain) ->
	<<State:32/big, Filler/binary>> = Remain,
	Ret = [{link_state, State}],
	if
		byte_size(Filler) > 0 ->
			{undefined, [{filler, Filler}|Ret]};
		true ->
			{undefined, Ret}
	end;
parse_m2pa_msgt(?M2PA_CLASS_M2PA, ?M2PA_TYPE_USER, Len, RemainIn) ->
	<<Pri:1, _:7, Mtp3Bin/binary>> = RemainIn,
	Mtp3 = mtp3_codec:parse_mtp3_msg(Mtp3Bin),
	{Mtp3, []}.

parse_msg(DataBin) when is_binary(DataBin) ->
	<<1:8, 0:8, MsgClass:8, MsgType:8, MsgLen:32/big, AllRemain/binary>> = DataBin,
	<<_:8, BSN:24/big, _:8, FSN:24/big, Remain/binary>> = AllRemain,
	{Mtp3, Params} = parse_m2pa_msgt(MsgClass, MsgType, MsgLen, Remain),
	{ok, #m2pa_msg{msg_class = MsgClass, msg_type = MsgType,
			fwd_seq_nr = FSN, back_seq_nr = BSN,
			mtp3 = Mtp3, parameters = Params}}.

encode_m2pa_msgt(?M2PA_CLASS_M2PA, ?M2PA_TYPE_USER, Mtp3, _Params) when is_binary(Mtp3) ->
	<<Mtp3/binary>>;
encode_m2pa_msgt(?M2PA_CLASS_M2PA, ?M2PA_TYPE_USER, Mtp3, Params) when is_record(Mtp3, mtp3_msg) ->
	Mtp3bin = mtp3_codec:encode_mtp3_msg(Mtp3),
	encode_m2pa_msgt(?M2PA_CLASS_M2PA, ?M2PA_TYPE_USER, Mtp3bin, Params);
encode_m2pa_msgt(?M2PA_CLASS_M2PA, ?M2PA_TYPE_LINK, _, Params) ->
	State = proplists:get_value(link_state, Params),
	% FIXME: filler
	Filler = <<>>,
	<<State:32/big, Filler/binary>>.


encode_msg(Msg) when is_record(Msg, m2pa_msg) ->
	#m2pa_msg{msg_class = MsgClass, msg_type = MsgType, fwd_seq_nr = FSN,
			back_seq_nr = BSN, mtp3 = Mtp3, parameters = Params} = Msg,
	Payload = encode_m2pa_msgt(MsgClass, MsgType, Mtp3, Params),
	MsgLen = byte_size(Payload) + 16,
	<<1:8, 0:8, MsgClass:8, MsgType:8, MsgLen:32/big, 0:8, BSN:24/big, 0:8, FSN:24/big, Payload/binary>>.
