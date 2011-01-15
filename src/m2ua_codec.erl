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

% parse a binary chunk of options into an options proplist
parse_m2ua_opts(<<>>, OptList) ->
	OptList;
parse_m2ua_opts(OptBin, OptList) ->
	<<Tag:16/big, LengthIncHdr:16/big, Remain/binary>> = OptBin,
	Length = LengthIncHdr - 4,
	<<Value:Length/binary, NextOpts/binary>> = Remain,
	NewOpt = {Tag, {Length, Value}},
	parse_m2ua_opts(NextOpts, [NewOpt|OptList]).

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
	<<OptNum:16/big, LengthIncHdr:16/big, DataBin/binary>>.

% encode a list of options
encode_m2ua_opts([], OptEnc) ->
	list_to_binary(OptEnc);
encode_m2ua_opts([CurOpt|OptPropList], OptEnc) ->
	CurOptEnc = encode_m2ua_opt(CurOpt),
	encode_m2ua_opts(OptPropList, list_to_binary([OptEnc, CurOptEnc])).
	

% encode a particular message type
encode_m2ua_msgt(MsgClass, MsgType, Params) ->
	OptBin = encode_m2ua_opts(Params, <<>>),
	MsgLenIncHdr = 4 + binary:byte_size(OptBin),
	<<1:8, 0:8, MsgClass:8, MsgType:8, MsgLenIncHdr:32/big, OptBin/binary>>.

% encode a message from record to binary
encode_m2ua_msg(#m2ua_msg{msg_class = MsgClass, msg_type = MsgType, parameters = Params}) ->
	encode_m2ua_msgt(MsgClass, MsgType, Params).
