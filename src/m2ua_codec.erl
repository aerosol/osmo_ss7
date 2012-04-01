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
-include("xua.hrl").
-include("m2ua.hrl").

-export([parse_m2ua_msg/1, encode_m2ua_msg/1]).

% parse a M2UA message binary into a record
parse_m2ua_msg(DataBin) when is_binary(DataBin) ->
	xua_codec:parse_msg(DataBin).

% encode a message from record to binary
encode_m2ua_msg(Msg) when is_record(Msg, xua_msg) ->
	xua_codec:encode_msg(Msg).
