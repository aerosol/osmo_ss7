% GSM TS 08.08 / 3GPP TS 48.008 BSSMAP

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

-module(bssmap_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("bssmap.hrl").

-export([parse_bssmap_msg/1, encode_bssmap_msg/1]).

parse_bssmap_msg(<<MsgType:8, Remain/binary>>) ->
	parse_bssmap_msgt(MsgType, Remain).

parse_bssmap_msgt(MsgType, Msg) when is_integer(MsgType), is_binary(Msg) ->
	IeList = parse_ies(Msg, []),
	{bssmap_msg, MsgType, IeList}.

parse_ies(<<>>, ParsedIeList) ->
	ParsedIeList;
parse_ies(Msg, ParsedIeList) when is_binary(Msg) ->
	CurIe = binary:first(Msg),
	% Parse current IE and append it to list of Parsed IEs
	case is_tv_ie(CurIe) of
		true ->
			Res = parse_ie_tv(CurIe, Msg);
		false ->
			Res = parse_ie(CurIe, Msg)
	end,
	{ok, BytesConsumed, ParsedIe} = Res,
	{CurIe, Payload} = ParsedIe,
	DecodedIe = decode_ie(CurIe, Payload),
	ParsedIeList1 = ParsedIeList ++ [DecodedIe],
	%ParsedIeList1 = ParsedIeList ++ [ParsedIe],
	RemainMsg = binary:part(Msg, BytesConsumed, byte_size(Msg)-BytesConsumed),
	parse_ies(RemainMsg, ParsedIeList1).

% check if this element is of TV type
is_tv_ie(T) when 
	T == ?BSSMAP_IE_NUMBER_OF_MSS;
	T == ?BSSMAP_IE_PERIODICITY;
	T == ?BSSMAP_IE_EXTD_RES_IND;
	T == ?BSSMAP_IE_INTERF_BAND_TO_USE;
	T == ?BSSMAP_IE_RR_CAUSE;
	T == ?BSSMAP_IE_DLCI;
	T == ?BSSMAP_IE_DOWNLINK_DTX_FLAG;
	T == ?BSSMAP_IE_RESPONSE_RQST;
	T == ?BSSMAP_IE_RES_IND_METHOD;
	T == ?BSSMAP_IE_CM_INFO_T1;
	T == ?BSSMAP_IE_CHOSEN_CHANNEL;
	T == ?BSSMAP_IE_CIPH_RESP_MODE;
	T == ?BSSMAP_IE_TRACE_TYPE;
	T == ?BSSMAP_IE_TRACE_REFERENCE;
	T == ?BSSMAP_IE_FORWARD_INDICATOR;
	T == ?BSSMAP_IE_CHOSEN_ENCR_ALG;
	T == ?BSSMAP_IE_CIRCUIT_POOL;
	T == ?BSSMAP_IE_TIME_INDICATION;
	T == ?BSSMAP_IE_CUR_CHAN_TYPE_1;
	T == ?BSSMAP_IE_QUEUEING_IND;
	T == ?BSSMAP_IE_SPEECH_VERSION;
	T == ?BSSMAP_IE_ASS_REQUIREMENT;
	T == ?BSSMAP_IE_EMLPP_PRIORITY;
	T == ?BSSMAP_IE_CONFIG_EVO_INDI;
	T == ?BSSMAP_IE_LSA_ACCESS_CTRL_SUPPR ->
		true;
is_tv_ie(_T) ->
		false.

% Parser for any non-TLV and non-TV IEs
parse_ie(?BSSMAP_IE_CIRC_ID_CODE, Msg) ->
	<<?BSSMAP_IE_CIRC_ID_CODE:8, Cic:16/big>> = Msg,
	{ok, 3, {?BSSMAP_IE_CIRC_ID_CODE, Cic}};
parse_ie(?BSSMAP_IE_CONN_REL_RQSTED, Msg) ->
	<<?BSSMAP_IE_CONN_REL_RQSTED:8>> = Msg,
	{ok, 1, {?BSSMAP_IE_CONN_REL_RQSTED, 1}};
parse_ie(?BSSMAP_IE_RES_AVAIL, Msg) ->
	<<?BSSMAP_IE_RES_AVAIL:8, ResAvail:8/binary>> = Msg,
	{ok, 9, {?BSSMAP_IE_RES_AVAIL, ResAvail}};
parse_ie(?BSSMAP_IE_TOT_RES_ACCESS, Msg) ->
	<<?BSSMAP_IE_TOT_RES_ACCESS:8, ResAvail:4/binary>> = Msg,
	{ok, 5, {?BSSMAP_IE_TOT_RES_ACCESS, ResAvail}};
parse_ie(?BSSMAP_IE_TALKER_FLAG, Msg) ->
	<<?BSSMAP_IE_TALKER_FLAG:8>> = Msg,
	{ok, 1, {?BSSMAP_IE_TALKER_FLAG, 1}};
% Default: Parser for TLV IE
parse_ie(MsgType, Msg) ->
	<<MsgType:8, Length:8, Value:Length/binary, _/binary>> = Msg,
	{ok, 2+Length, {MsgType, Value}}.

% Parser for simple Tag-Value IE
parse_ie_tv(IeType, Msg) ->
	<<IeType:8, Par:8>> = Msg,
	{ok, 2, {IeType, Par}}.


% FIXME
encode_bssmap_msg(_) ->
	ok.





decode_ie(?BSSMAP_IE_CIRC_ID_CODE, <<Pcm:11, Ts:5>>) ->
	{circuit_id, Pcm, Ts};
decode_ie(?BSSMAP_IE_IMSI, Remain) ->
	{imsi, bin_bcd2str(Remain)};
decode_ie(?BSSMAP_IE_TMSI, <<Tmsi:32>>) ->
	{tmsi, Tmsi};
decode_ie(?BSSMAP_IE_L3_HDR_INFO, <<Pdisc:8, Tid:8>>) ->
	{l3_hdr_info, Pdisc, Tid};
decode_ie(?BSSMAP_IE_ENCR_INFO, <<Algos:8, Key/binary>>) ->
	{encr_info, Algos, Key};
decode_ie(?BSSMAP_IE_CHANNEL_TYPE, <<_:4, Spdi:4, RateType:8, Remain/binary>>) ->
	{chan_type, Spdi, RateType, Remain};
decode_ie(?BSSMAP_IE_EXTD_RES_IND, Ri) ->
	<<_:6, Sm:1, Tarr:1>> = <<Ri>>,
	{extended_ri, Sm, Tarr};
decode_ie(?BSSMAP_IE_TOT_RES_ACCESS, <<NumFr:16/big, NumHr:16/big>>) ->
	{tot_res_access, NumFr, NumHr};
decode_ie(?BSSMAP_IE_CELL_ID, <<_Spare:4, Discr:4, Remain/binary>>) ->
	{cell_id, decode_cid_ie(Discr, Remain)};
decode_ie(?BSSMAP_IE_PRIORITY, <<_:1, Pci:1, Prio:4, Qa:1, Pvi:1>>) ->
	{priority, Pci, Prio, Qa, Pvi};
decode_ie(?BSSMAP_IE_CELL_ID_LIST, <<_Spare:4, Discr:4, Remain/binary>>) ->
	{cell_id_list, decode_cid_list(Discr, Remain, [])};
decode_ie(?BSSMAP_IE_DIAGNOSTIC, <<ErrPtr:8, _:4, BitPtr:4, MsgRecv/binary>>) ->
	{diagnostic, ErrPtr, BitPtr, MsgRecv};
decode_ie(?BSSMAP_IE_CHOSEN_CHANNEL, Int) ->
	<<Mode:4, Chan:4>> = <<Int:8>>,
	{chosen_channel, Mode, Chan};
decode_ie(?BSSMAP_IE_MOBILE_IDENTITY, Data) ->
	% FIXME
	fixme;
% Default: don't decode
decode_ie(IeI, Data) ->
	{IeI, Data}.

decode_cid_ie(?CELL_ID_WHOLE_GLOBAL, Remain) ->
	<<Mcc2:4, Mcc1:4, Mnc3:4, Mcc3:4, Mnc2:4, Mnc1:4, Lac:16/big, Ci:16/big>> = Remain,
	[{mcc, [Mcc1, Mcc2, Mcc3]}, {mnc, [Mnc1, Mnc2, Mnc3]}, {lac, Lac}, {cid, Ci}];
decode_cid_ie(?CELL_ID_LAC_AND_CI, Remain) ->
	<<Lac:16/big, Ci:16/big>> = Remain,
	[{lac, Lac}, {cid, Ci}];
decode_cid_ie(?CELL_ID_CI, Remain) ->
	<<Ci:16/big>> = Remain,
	[{cid, Ci}];
decode_cid_ie(?CELL_ID_NO_CELL, _Remain) ->
	[];
decode_cid_ie(?CELL_ID_UTRAN_PLMN_LAC_RNC, Remain) ->
	<<Mcc2:4, Mcc1:4, Mnc3:4, Mcc3:4, Mnc2:4, Mnc1:4, Lac:16/big, Rnc:16/big>> = Remain,
	[{mcc, [Mcc1, Mcc2, Mcc3]}, {mnc, [Mnc1, Mnc2, Mnc3]}, {lac, Lac}, {rnc_id, Rnc}];
decode_cid_ie(?CELL_ID_UTRAN_RNC, Remain) ->
	<<Rnc:16/big>> = Remain,
	[{rnc_id, Rnc}];
decode_cid_ie(?CELL_ID_UTRAN_LAC_RNC, Remain) ->
	<<Lac:16/big, Rnc:16/big>> = Remain,
	[{lac, Lac}, {rnc_id, Rnc}].

decode_cid_list(Discr, Data, List) ->
	case Discr of
		?CELL_ID_WHOLE_GLOBAL -> Len = 7;
		?CELL_ID_LAC_AND_CI ->	 Len = 4;
		?CELL_ID_CI ->		 Len = 2;
		?CELL_ID_NO_CELL ->	 Len = 0;
		?CELL_ID_UTRAN_PLMN_LAC_RNC -> Len = 7;
		?CELL_ID_UTRAN_RNC ->	Len = 2;
		?CELL_ID_UTRAN_LAC_RNC -> Len = 4
	end,
	<<Subset:Len/binary, Remain/binary>> = Data,
	Elem = {cell_id, decode_cid_ie(Discr, Subset)},
	decode_cid_list(Discr, Remain, List ++ [Elem]).



bin_bcd2str(BcdBin) when is_binary(BcdBin) ->
	bin_bcd2str(BcdBin, []).
bin_bcd2str(<<>>, List) ->
	List;
bin_bcd2str(BcdBin, List) ->
	<<Nibble:4, Remain/bitstring>> = BcdBin,
	Char = $0 + Nibble,
	bin_bcd2str(Remain, List ++ [Char]).
