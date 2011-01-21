% 

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

-module(mgw_nat).
-author("Harald Welte <laforge@gnumonks.org>").
-export([mangle_rx_data/3]).

%-include_lib("kernel/include/inet.hrl").
%-include_lib("kernel/include/inet_sctp.hrl").

-include("m2ua.hrl").
-include("mtp3.hrl").
-include("isup.hrl").
-include("sccp.hrl").

% mangle the received data
mangle_rx_data(L, From, Data) when is_binary(Data) ->
	{ok, M2ua} = m2ua_codec:parse_m2ua_msg(Data),
	%io:format("M2UA Decode: ~p~n", [M2ua]),
	case M2ua of
		#m2ua_msg{msg_class = ?M2UA_MSGC_MAUP,
			  msg_type = ?M2UA_MAUP_MSGT_DATA} ->
			M2ua_out = mangle_rx_m2ua_maup(L, From, M2ua);
		#m2ua_msg{} ->
			% simply pass it along unmodified
			M2ua_out = M2ua
	end,
	% re-encode the data
	%io:format("M2UA Encode: ~p~n", [M2ua_out]),
	m2ua_codec:encode_m2ua_msg(M2ua_out).

% mangle the received M2UA
mangle_rx_m2ua_maup(L, From, M2ua = #m2ua_msg{parameters = Params}) ->
	{_Len, M2uaPayload} = proplists:get_value(16#300, Params),
	Mtp3 = mtp3_codec:parse_mtp3_msg(M2uaPayload),
	%io:format("MTP3 Decode: ~p~n", [Mtp3]),
	Mtp3_out = mangle_rx_mtp3(L, From, Mtp3),
	%io:format("MTP3 Encode: ~p~n", [Mtp3_out]),
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
	io:format("ISUP In: ~p~n", [Payload]),
	Isup = isup_codec:parse_isup_msg(Payload),
	io:format("ISUP Decode: ~p~n", [Isup]),
	% FIXME
	%mangle_rx_isup(From, Isup#isup_msg.msg_type, Isup),
	case Isup#isup_msg.msg_type of
		?ISUP_MSGT_IAM ->
			io:format("ISUP Encode In: ~p~n", [Isup]),
			Isup_out = isup_codec:encode_isup_msg(Isup),
			io:format("ISUP Encode Out: ~p~n", [Isup_out]),
			% FIXME
			if Isup_out == Payload -> ok;
			   true -> io:format("ISUP DATA NOT EQUAL!~n")
			end,
			% return modified MTP3 payload
			Mtp3#mtp3_msg{payload = Isup_out};
		_ ->
			% return UNmodified MTP3 payload
			Mtp3
	end;
% mangle the SCCP content
mangle_rx_mtp3_serv(L, From, ?MTP3_SERV_SCCP, Mtp3 = #mtp3_msg{payload = Payload}) ->
	Sccp = sccp_codec:parse_sccp_msg(Payload),
	io:format("SCCP Decode: ~p~n", [Sccp]),
	% FIXME
	Mtp3;
% default: do nothing
mangle_rx_mtp3_serv(_L, _From, _, Mtp3) ->
	Mtp3.

-define(MSRN_PFX_MSC,	[8,9,0,9,9]).
-define(MSRN_PFX_STP,	[9,2,9,9,4,2,0,0]).

mangle_rx_isup(From, MsgType, Msg = #isup_msg{parameters = Params}) when
				  MsgType == ?ISUP_MSGT_IAM	->
	CalledNum = proplists:get_value(?ISUP_PAR_CALLED_P_NUM, Params),
	DigitsIn = CalledNum#party_number.phone_number,
	Last2DigF = lists:sublist(DigitsIn, length(DigitsIn)-2, 3),
	case From of
		from_stp ->
			DigitsOut = ?MSRN_PFX_MSC ++ Last2DigF,
			io:format("IAM MSRN rewrite (MSC->STP): ~p -> ~p~n",
				  [DigitsIn, DigitsOut]);
		from_msc ->
			DigitsOut = DigitsIn,
			io:format("No support for MSC->STP MSRN rewrite~n")
	end,
	CalledNumOut = CalledNum#party_number{phone_number=DigitsOut},
	ParamsDel = proplists:delete(?ISUP_PAR_CALLED_P_NUM, Params),
	ParamsOut = [{?ISUP_PAR_CALLED_P_NUM, CalledNumOut}|ParamsDel],
	#isup_msg{parameters = ParamsOut};
% default case: no mangling
mangle_rx_isup(_From, _Type, Msg) when is_record(Msg, isup_msg) ->
	Msg.
