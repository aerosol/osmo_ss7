% RFC 3331 MTP2 User Adaption Layer coding / decoding

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
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

-module(m2ua_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("m2ua.hrl").

-export([parse_m2ua_msg/1, encode_m2ua_msg/1]).

% compute the number of pad bits required after a binary parameter
get_num_pad_bytes(BinLenBytes) ->
	case BinLenBytes rem 4 of
		0 ->	0;
		Val -> 	4 - Val
	end.

% parse a binary chunk of options into an options proplist
parse_m2ua_opts(<<>>, OptList) when is_list(OptList) ->
	OptList;
parse_m2ua_opts(OptBin, OptList) when is_list(OptList) ->
	<<Tag:16/big, LengthIncHdr:16/big, Remain/binary>> = OptBin,
	Length = LengthIncHdr - 4,
	PadLength = get_num_pad_bytes(Length),
	%io:format("Tag ~w, LenInHdr ~w, Len ~w, PadLen ~w, Remain ~w(~p)~n",
	%	  [Tag, LengthIncHdr, Length, PadLength, byte_size(Remain), Remain]),
	<<Value:Length/binary, PadNextOpts/binary>> = Remain,
	% this is ridiculous, we cannot use "<<Value:Length/binary,
	% 0:PadLength, Remain/binary>>" as the last part would not match an
	% empty binary <<>> anymore.  Without the "0:PadLengh" this works
	% perfectly fine.  Now we need some complicated construct and check if
	% the resulting list would be empty :((
	if
		byte_size(PadNextOpts) > PadLength ->
			<<0:PadLength/integer-unit:8, NextOpts/binary>> = PadNextOpts;
		true ->
			NextOpts = <<>>
	end,
	NewOpt = {Tag, {Length, Value}},
	parse_m2ua_opts(NextOpts, OptList ++ [NewOpt]).

% parse a single M2UA message
parse_m2ua_msgt(_, _, _, Remain) ->
	parse_m2ua_opts(Remain, []).

% parse a M2UA message binary into a record
parse_m2ua_msg(DataBin) when is_binary(DataBin) ->
	<<1:8, 0:8, MsgClass:8, MsgType:8, MsgLen:32/big, Remain/binary>> = DataBin,
	Parsed = parse_m2ua_msgt(MsgClass, MsgType, MsgLen, Remain),
	{ok, #m2ua_msg{msg_class = MsgClass, msg_type = MsgType, parameters = Parsed}}.



% encode a single option
encode_m2ua_opt({OptNum, {DataBinLen, DataBin}}) when is_integer(OptNum) ->
	LengthIncHdr = DataBinLen + 4,
	PadLength = get_num_pad_bytes(DataBinLen),
	case PadLength of
		0 -> <<OptNum:16/big, LengthIncHdr:16/big, DataBin/binary>>;
		_ -> <<OptNum:16/big, LengthIncHdr:16/big, DataBin/binary, 0:PadLength/integer-unit:8>>
	end.

% encode a list of options
encode_m2ua_opts([], OptEnc) ->
	OptEnc;
encode_m2ua_opts([CurOpt|OptPropList], OptEnc) ->
	CurOptEnc = encode_m2ua_opt(CurOpt),
	encode_m2ua_opts(OptPropList, <<OptEnc/binary, CurOptEnc/binary>>).
	

% encode a particular message type
encode_m2ua_msgt(MsgClass, MsgType, Params) ->
	OptBin = encode_m2ua_opts(Params, <<>>),
	MsgLenIncHdr = 8 + byte_size(OptBin),
	<<1:8, 0:8, MsgClass:8, MsgType:8, MsgLenIncHdr:32/big, OptBin/binary>>.

% encode a message from record to binary
encode_m2ua_msg(#m2ua_msg{msg_class = MsgClass, msg_type = MsgType, parameters = Params}) ->
	encode_m2ua_msgt(MsgClass, MsgType, Params).
