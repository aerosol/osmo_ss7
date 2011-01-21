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
	IsupMangled = mangle_rx_isup(From, Isup#isup_msg.msg_type, Isup),
	if IsupMangled == Isup ->
		Mtp3;
	   true ->
		io:format("ISUP Encode In: ~p~n", [IsupMangled]),
		Payload_out = isup_codec:encode_isup_msg(IsupMangled),
		io:format("ISUP Encode Out: ~p~n", [Payload_out]),
		% return modified MTP3 payload
		Mtp3#mtp3_msg{payload = Payload_out}
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual mangling of the decoded ISUP messages 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MSRN_PFX_MSC,	[8,9,0,9,9]).
%-define(MSRN_PFX_MSC,	[0,0,3,5,4,8,9,0,9,9]).
-define(MSRN_PFX_STP,	[9,2,9,9,4,2,0,0]).
-define(INTERN_PFX,	[6,3]).

% iterate over list of parameters and call mangle_rx_isup_par() for each one
mangle_rx_isup_params(_From, _MsgType, _Msg, ParListOut, []) ->
	ParListOut;
mangle_rx_isup_params(From, MsgType, Msg, ParListOut, [Par|ParList]) ->
	ParOut = mangle_rx_isup_par(From, MsgType, Msg, Par),
	mangle_rx_isup_params(From, MsgType, Msg, ParListOut++[ParOut], ParList).

% manipulate phone numbers
mangle_rx_isup_par(From, MsgType, Msg, {ParType, ParBody}) when
					ParType == ?ISUP_PAR_CALLED_P_NUM;
					ParType == ?ISUP_PAR_CALLING_P_NUM ->
	NewParBody = mangle_isup_number(From, MsgType, ParType, ParBody),
	{ParType, NewParBody};
% defauly case: do not mangle this parameter
mangle_rx_isup_par(_From, _MsgType, _Msg, Par) ->
	Par.

% mangle an incoming ISUP message
mangle_rx_isup(From, MsgType, Msg = #isup_msg{parameters = Params}) ->
	ParamsOut = mangle_rx_isup_params(From, MsgType, Msg, [], Params),
	% return message with modified parameter list
	Msg#isup_msg{parameters = ParamsOut}.

% STP->MSC: Mangle a Party Number in IAM
mangle_isup_number(from_stp, ?ISUP_MSGT_IAM, NumType, PartyNum) ->
	case NumType of
		?ISUP_PAR_CALLED_P_NUM ->
			io:format("IAM MSRN rewrite (STP->MSC): "),
			Num1 = isup_party_replace_prefix(PartyNum, ?MSRN_PFX_STP, ?MSRN_PFX_MSC),
			% if there was no MSRN rewrite, internationalize it
			if Num1 == PartyNum ->
				isup_party_internationalize(Num1, ?INTERN_PFX);
			   true ->
				Num1
			end;
		_ ->
			PartyNum
	end;
% MSC->STP: Mangle connected number in response to IAM
mangle_isup_number(from_msc, MsgT, NumType, PartyNum) when MsgT == ?ISUP_MSGT_CON;
							   MsgT == ?ISUP_MSGT_ANM ->
	case NumType of
		?ISUP_PAR_CONNECTED_NUM ->
			io:format("CON MSRN rewrite (MSC->STP): "),
			Num1 = isup_party_replace_prefix(PartyNum, ?MSRN_PFX_MSC, ?MSRN_PFX_STP),
			if Num1 == PartyNum ->
				isup_party_nationalize(Num1, ?INTERN_PFX);
			   true ->
				Num1
			end;
		_ ->
			PartyNum
	end;
% MAC->STP: Mangle IAM international -> national
mangle_isup_number(from_msc, ?ISUP_MSGT_IAM, NumType, PartyNum) ->
	case NumType of
		?ISUP_PAR_CALLED_P_NUM ->
			isup_party_nationalize(PartyNum, ?INTERN_PFX);
		_ ->
			PartyNum
	end;
% STP->MSC: Mangle connected number in response to IAM (national->international)
mangle_isup_number(from_stp, MsgT, NumType, PartyNum) when MsgT == ?ISUP_MSGT_CON;
							   MsgT == ?ISUP_MSGT_ANM ->
	case NumType of
		?ISUP_PAR_CONNECTED_NUM ->
			isup_party_internationalize(PartyNum, ?INTERN_PFX);
		_ ->
			PartyNum
	end;
% default case: no rewrite
mangle_isup_number(from_msc, _, _, PartyNum) ->
	PartyNum.

% replace the prefix of PartyNum with NewPfx _if_ the current prefix matches MatchPfx
isup_party_replace_prefix(PartyNum, MatchPfx, NewPfx) ->
	DigitsIn = PartyNum#party_number.phone_number,
	MatchPfxLen = length(MatchPfx),
	Pfx = lists:sublist(DigitsIn, 1, MatchPfxLen),
	if Pfx == MatchPfx ->
		Trailer = lists:sublist(DigitsIn, MatchPfxLen+1, length(DigitsIn)-MatchPfxLen),
		DigitsOut = NewPfx ++ Trailer,
		io:format("Prefix rewrite: ~p -> ~p~n", [DigitsIn, DigitsOut]);
	   true ->
		io:format("Prefix rewrite: NO MATCH (~p != ~p)~n", [Pfx, MatchPfx]),
		DigitsOut = DigitsIn
	end,
	PartyNum#party_number{phone_number = DigitsOut}.

isup_party_internationalize(PartyNum, CountryCode) ->
	#party_number{phone_number = DigitsIn, nature_of_addr_ind = Nature} = PartyNum,
	case Nature of
		?ISUP_ADDR_NAT_NATIONAL ->
			DigitsOut = CountryCode ++ DigitsIn,
			NatureOut = ?ISUP_ADDR_NAT_INTERNATIONAL,
			io:format("Internationalize: ~p -> ~p~n", [DigitsIn, DigitsOut]);
		_ ->
			DigitsOut = DigitsIn,
			NatureOut = Nature
	end,
	PartyNum#party_number{phone_number = DigitsOut, nature_of_addr_ind = NatureOut}.

isup_party_nationalize(PartyNum, CountryCode) ->
	#party_number{phone_number = DigitsIn, nature_of_addr_ind = Nature} = PartyNum,
	CountryCodeLen = length(CountryCode),
	case Nature of
		?ISUP_ADDR_NAT_INTERNATIONAL ->
			Pfx = lists:sublist(DigitsIn, CountryCodeLen),
			if Pfx == CountryCode ->
				DigitsOut = lists:sublist(DigitsIn, CountryCodeLen+1,
							  length(DigitsIn)-CountryCodeLen),
				NatureOut = ?ISUP_ADDR_NAT_NATIONAL,
				io:format("Nationalize: ~p -> ~p~n", [DigitsIn, DigitsOut]);
			   true ->
				DigitsOut = DigitsIn,
				NatureOut = Nature
			end;
		_ ->
			DigitsOut = DigitsIn,
			NatureOut = Nature
	end,
	PartyNum#party_number{phone_number = DigitsOut, nature_of_addr_ind = NatureOut}.
