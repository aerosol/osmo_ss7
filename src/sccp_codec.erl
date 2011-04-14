% ITU-T Q.71x SCCP Message coding / decoding

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

-module(sccp_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("sccp.hrl").

-export([parse_sccp_msg/1, encode_sccp_msg/1, encode_sccp_msgt/2]).

-compile(export_all).

-compile({parse_transform, exprecs}).
-export_records([global_title, sccp_addr, sccp_msg]).

parse_point_code(BinPC, PCind) when is_binary(BinPC) ->
	case PCind of
		1 ->
			<<PointCode:16/big, Remain/binary>> = BinPC;
		_ ->
			Remain = BinPC,
			PointCode = undef
	end,
	{Remain, PointCode}.

parse_ssn(BinSSN, SSNind) ->
	case SSNind of
		1 ->
			<<SSN:8, Remain/binary>> = BinSSN;
		_ ->
			Remain = BinSSN,
			SSN = undef
	end,
	{Remain, SSN}.

enc_is_odd(Enc) ->
	case Enc of
		1 -> 1;
		_ -> 0
	end.

parse_gt(BinGT, GTind) ->
	case GTind of
		?SCCP_GTI_NO_GT ->
			undef;
		?SCCP_GTI_NAT_ONLY ->
			% Figure 7/Q.713
			<<Odd:1, Nature:7, Digits/binary>> = BinGT,
			PhoneNum = isup_codec:parse_isup_party(Digits, Odd),
			#global_title{gti = GTind,
				      nature_of_addr_ind = Nature,
				      phone_number = PhoneNum};
		?SCCP_GTI_TT_ONLY ->
			% Figure 9/Q.913
			<<TransType:8, Digits/binary>> = BinGT,
			% Used in national interfaces only, we cannot parse Digits
			#global_title{gti = GTind,
				      trans_type = TransType,
				      phone_number = Digits};
		?SCCP_GTI_TT_NP_ENC ->
			% Figure 10/Q.713
			<<TransType:8, NumPlan:4, Enc:4, Digits/binary>> = BinGT,
			PhoneNum = isup_codec:parse_isup_party(Digits, enc_is_odd(Enc)),
			#global_title{gti = GTind,
				      trans_type = TransType, encoding = Enc, 
				      numbering_plan = NumPlan,
				      phone_number = PhoneNum};
		?SCCP_GTI_TT_NP_ENC_NAT ->
			% Figure 11/Q.713
			<<TransType:8, NumPlan:4, Enc:4, 0:1, Nature:7, Digits/binary>> = BinGT,
			PhoneNum = isup_codec:parse_isup_party(Digits, enc_is_odd(Enc)),
			#global_title{gti = GTind,
				      trans_type = TransType, encoding = Enc,
				      numbering_plan = NumPlan,
				      nature_of_addr_ind = Nature,
				      phone_number = PhoneNum};
		_ ->
			BinGT
	end.

% parse SCCP Address
parse_sccp_addr(BinAddr) when is_binary(BinAddr) ->
	<<ResNatUse:1, RoutInd:1, GTind:4, SSNind:1, PCind:1, Remain/binary>> = BinAddr,
	{RemainPC, OptPC} = parse_point_code(Remain, PCind),
	{RemainSSN, OptSSN} = parse_ssn(RemainPC, SSNind),
	OptGT = parse_gt(RemainSSN, GTind),
	#sccp_addr{res_nat_use = ResNatUse, route_on_ssn = RoutInd,
		   point_code = OptPC, ssn = OptSSN, global_title = OptGT}.

% parse SCCP Optional Part
parse_sccp_opt(OptType, OptLen, Content) ->
	{OptType, {OptLen, Content}}.

parse_sccp_opts(<<>>, OptList) ->
	% empty list
	OptList;
parse_sccp_opts(<<0>>, OptList) ->
	% end of options
	OptList;
parse_sccp_opts(OptBin, OptList) ->
	<<OptType, OptLen, Content:OptLen/binary, Remain/binary>> = OptBin,
	NewOpt = parse_sccp_opt(OptType, OptLen, Content),
	parse_sccp_opts(Remain, [NewOpt|OptList]).

% Parse incoming SCCP message, one function for every message type
parse_sccp_msgt(?SCCP_MSGT_CR, DataBin) ->
	% first get the fixed part
	<<_:8, SrcLocalRef:24/big, ProtoClass:8, RemainVar/binary >> = DataBin,
	% variable length fixed part
	<<PtrVar:8, PtrOpt:8, _/binary>> = RemainVar,
	CalledPartyLen = binary:at(RemainVar, PtrVar),
	CalledParty = binary:part(RemainVar, PtrVar+1, CalledPartyLen),
	CalledPartyDec = parse_sccp_addr(CalledParty),
	% optional part
	OptBin = binary:part(RemainVar, 1 + PtrOpt, byte_size(RemainVar)-(1+PtrOpt)),
	OptList = parse_sccp_opts(OptBin, []),
	%OptList = [],
	% build parsed list of message
	[{src_local_ref, SrcLocalRef},{protocol_class, ProtoClass},{called_party_addr, CalledPartyDec}|OptList];
parse_sccp_msgt(?SCCP_MSGT_CC, DataBin) ->
	% first get the fixed part
	<<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, ProtoClass:8, Remain/binary >> = DataBin,
	% optional part
	OptList = parse_sccp_opts(Remain, []),
	% build parsed list of message
	[{dst_local_ref, DstLocalRef},{src_local_ref, SrcLocalRef},{protocol_class, ProtoClass}|OptList];
parse_sccp_msgt(?SCCP_MSGT_CREF, DataBin) ->
	% first get the fixed part
	<<_:8, DstLocalRef:24/big, RefusalCause:8, Remain/binary >> = DataBin,
	% optional part
	OptList = parse_sccp_opts(Remain, []),
	% build parsed list of message
	[{dst_local_ref, DstLocalRef},{refusal_cause, RefusalCause}|OptList];
parse_sccp_msgt(?SCCP_MSGT_RLSD, DataBin) ->
	<<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, ReleaseCause:8, Remain/binary >> = DataBin,
	% optional part
	OptList = parse_sccp_opts(Remain, []),
	% build parsed list of message
	[{dst_local_ref, DstLocalRef},{src_local_ref, SrcLocalRef},{release_cause, ReleaseCause}|OptList];
parse_sccp_msgt(?SCCP_MSGT_RLC, DataBin) ->
	<<_:8, DstLocalRef:24/big, SrcLocalRef:24/big>> = DataBin,
	% build parsed list of message
	[{dst_local_ref, DstLocalRef},{src_local_ref, SrcLocalRef}];
parse_sccp_msgt(?SCCP_MSGT_DT1, DataBin) ->
	<<_:8, DstLocalRef:24/big, SegmReass:8, DataPtr:8, Remain/binary >> = DataBin,
	DataLen = binary:at(Remain, DataPtr-1),
	UserData = binary:part(Remain, DataPtr-1+1, DataLen),
	% build parsed list of message
	[{dst_local_ref, DstLocalRef},{segm_reass, SegmReass},{user_data, UserData}];
parse_sccp_msgt(?SCCP_MSGT_DT2, DataBin) ->
	<<_:8, DstLocalRef:24/big, SeqSegm:16, DataPtr:8, Remain/binary >> = DataBin,
	DataLen = binary:at(Remain, DataPtr-1),
	UserData = binary:part(Remain, DataPtr-1+1, DataLen),
	% build parsed list of message
	[{dst_local_ref, DstLocalRef},{seq_segm, SeqSegm},{user_data, UserData}];
parse_sccp_msgt(?SCCP_MSGT_AK, DataBin) ->
	<<_:8, DstLocalRef:24/big, RxSeqnr:8, Credit:8>> = DataBin,
	[{dst_local_ref, DstLocalRef},{rx_seq_nr, RxSeqnr},{credit, Credit}];
parse_sccp_msgt(?SCCP_MSGT_UDT, DataBin) ->
	<<_:8, ProtoClass:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary >> = DataBin,
	% variable part
	CalledPartyLen = binary:at(Remain, CalledPartyPtr-3),
	CalledParty = binary:part(Remain, CalledPartyPtr-3+1, CalledPartyLen),
	CalledPartyDec = parse_sccp_addr(CalledParty),
	CallingPartyLen = binary:at(Remain, CallingPartyPtr-2),
	CallingParty = binary:part(Remain, CallingPartyPtr-2+1, CallingPartyLen),
	CallingPartyDec = parse_sccp_addr(CallingParty),
	DataLen = binary:at(Remain, DataPtr-1),
	UserData = binary:part(Remain, DataPtr-1+1, DataLen),
	[{protocol_class, ProtoClass},{called_party_addr, CalledPartyDec},
	 {calling_party_addr, CallingPartyDec},{user_data, UserData}];
parse_sccp_msgt(?SCCP_MSGT_UDTS, DataBin) ->
	<<_:8, ReturnCause:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary >> = DataBin,
	% variable part
	CalledPartyLen = binary:at(Remain, CalledPartyPtr-3),
	CalledParty = binary:part(Remain, CalledPartyPtr-3+1, CalledPartyLen),
	CalledPartyDec = parse_sccp_addr(CalledParty),
	CallingPartyLen = binary:at(Remain, CallingPartyPtr-2),
	CallingParty = binary:part(Remain, CallingPartyPtr-2+1, CallingPartyLen),
	CallingPartyDec = parse_sccp_addr(CallingParty),
	DataLen = binary:at(Remain, DataPtr-1),
	UserData = binary:part(Remain, DataPtr-1+1, DataLen),
	[{return_cause, ReturnCause},{called_party_addr, CalledPartyDec},
	 {calling_party_addr, CallingPartyDec},{user_data, UserData}];
parse_sccp_msgt(?SCCP_MSGT_ED, DataBin) ->
	<<_:8, DstLocalRef:24/big, DataPtr:8, Remain/binary>> = DataBin,
	DataLen = binary:at(Remain, DataPtr-1),
	UserData = binary:part(Remain, DataPtr-1+1, DataLen),
	[{dst_local_ref, DstLocalRef}, {user_data, UserData}];
parse_sccp_msgt(?SCCP_MSGT_EA, DataBin) ->
	<<_:8, DstLocalRef:24/big>> = DataBin,
	[{dst_local_ref, DstLocalRef}];
parse_sccp_msgt(?SCCP_MSGT_RSR, DataBin) ->
	<<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, ResetCause:8>> = DataBin,
	[{dst_local_ref, DstLocalRef},{src_local_ref, SrcLocalRef},{reset_cause, ResetCause}];
parse_sccp_msgt(?SCCP_MSGT_RSC, DataBin) ->
	<<_:8, DstLocalRef:24/big, SrcLocalRef:24/big>> = DataBin,
	[{dst_local_ref, DstLocalRef},{src_local_ref, SrcLocalRef}];
parse_sccp_msgt(?SCCP_MSGT_ERR, DataBin) ->
	<<_:8, DstLocalRef:24/big, ErrCause:8>> = DataBin,
	[{dst_local_ref, DstLocalRef},{error_cause, ErrCause}];
parse_sccp_msgt(?SCCP_MSGT_IT, DataBin) ->
	<<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, ProtoClass:8, SegmSeq:16, Credit:8>> = DataBin,
	[{dst_local_ref, DstLocalRef},{src_local_ref, SrcLocalRef},
         {protocol_class, ProtoClass},{seq_segm, SegmSeq},{credit, Credit}].
% FIXME: XUDT/XUDTS, LUDT/LUDTS

% process one incoming SCCP message
parse_sccp_msg(DataBin) ->
	MsgType = binary:first(DataBin),
	Parsed = parse_sccp_msgt(MsgType, DataBin),
	{ok, #sccp_msg{msg_type = MsgType, parameters = Parsed}}.

% Encoding Part

gt_enc_by_odd(Odd) ->
	if Odd == 1 ->
		1;
	   true ->
		2
	end.

encode_gt(undefined) ->
	{?SCCP_GTI_NO_GT, <<>>};
encode_gt(#global_title{gti = GTind, phone_number = PhoneNum,
			nature_of_addr_ind = Nature,
			trans_type = TransType, encoding = _EncOrig,
			numbering_plan = NumPlan}) ->
	case GTind of
		?SCCP_GTI_NO_GT ->
			{GTind, <<>>};
		?SCCP_GTI_NAT_ONLY ->
			% Figure 7/Q.713
			{PhoneBin, OddEven} = isup_codec:encode_isup_party(PhoneNum),
			{GTind, <<OddEven:1, Nature:7, PhoneBin/binary>>};
		?SCCP_GTI_TT_ONLY ->
			% Figure 9/Q.913
			% Used in national interfaces only, we cannot parse Digits
			{GTind, <<TransType:8, PhoneNum/binary>>};
		?SCCP_GTI_TT_NP_ENC ->
			% Figure 10/Q.713
			{PhoneBin, OddEven} = isup_codec:encode_isup_party(PhoneNum),
			Enc = gt_enc_by_odd(OddEven),
			{GTind, <<TransType:8, NumPlan:4, Enc:4, PhoneBin/binary>>};
		?SCCP_GTI_TT_NP_ENC_NAT ->
			% Figure 11/Q.713
			{PhoneBin, OddEven} = isup_codec:encode_isup_party(PhoneNum),
			Enc = gt_enc_by_odd(OddEven),
			{GTind, <<TransType:8, NumPlan:4, Enc:4, 0:1, Nature:7, PhoneBin/binary>>}
	end.

encode_pc(PointCode) ->
	case PointCode of
		undef ->
			{0, <<>>};
		_ ->
			{1, <<PointCode:16/big>>}
	end.

encode_ssn(SSN) ->
	case SSN of
		undef ->
			{0, <<>>};
		_ ->
			{1, <<SSN:8>>}
	end.

undef_or_true(Foo) ->
	case Foo of
		undefined -> 0;
		0 -> 0;
		_ -> 1
	end.


encode_sccp_addr(#sccp_addr{res_nat_use = ResNatUse,
			    route_on_ssn = RoutInd,
			    point_code = PointCode,
			    ssn = SSN,
			    global_title = GT}) ->

	{GTind, GTbin} = encode_gt(GT),
	{SSNind, SSNbin} = encode_ssn(SSN),
	{PCind, PCbin} = encode_pc(PointCode),
	ResNatOut = undef_or_true(ResNatUse),
	RoutIndOut = undef_or_true(RoutInd),
	<<ResNatOut:1, RoutIndOut:1, GTind:4, SSNind:1, PCind:1, PCbin/binary, SSNbin/binary, GTbin/binary>>.


encode_sccp_opt({OptNum, {DataBinLen, DataBin}}) when is_integer(OptNum) ->
	DataBinLen8 = DataBinLen*8,
	<<OptNum:8, DataBinLen:8, DataBin:DataBinLen8>>;
encode_sccp_opt({OptAtom,_}) when is_atom(OptAtom) ->
	<<>>.

encode_sccp_opts([], OptEnc) ->
	% end of options + convert to binary
	list_to_binary([OptEnc, ?SCCP_PNC_END_OF_OPTIONAL]);
encode_sccp_opts([CurOpt|OptPropList], OptEnc) ->
	CurOptEnc = encode_sccp_opt(CurOpt),
	encode_sccp_opts(OptPropList, list_to_binary([OptEnc,CurOptEnc])).

	

encode_sccp_msgt(?SCCP_MSGT_CR, Params) ->
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	ProtoClass = proplists:get_value(protocol_class, Params),
	OptBin = encode_sccp_opts(Params, []),
	<<?SCCP_MSGT_CR:8, SrcLocalRef:24/big, ProtoClass:8, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_CC, Params) ->
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	ProtoClass = proplists:get_value(protocol_class, Params),
	OptBin = encode_sccp_opts(Params, []),
	<<?SCCP_MSGT_CC:8, DstLocalRef:24/big, SrcLocalRef:24/big, ProtoClass:8, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_CREF, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	RefusalCause = proplists:get_value(refusal_cause, Params),
	OptBin = encode_sccp_opts(Params, []),
	<<?SCCP_MSGT_CREF:8, DstLocalRef:24/big, RefusalCause:8, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_RLSD, Params) ->
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	ReleaseCause = proplists:get_value(release_cause, Params),
	OptBin = encode_sccp_opts(Params, []),
	<<?SCCP_MSGT_RLSD:8, DstLocalRef:24/big, SrcLocalRef:24/big, ReleaseCause:8, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_RLC, Params) ->
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	<<?SCCP_MSGT_RLC:8, DstLocalRef:24/big, SrcLocalRef:24/big>>;
encode_sccp_msgt(?SCCP_MSGT_DT1, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	SegmReass = proplists:get_value(segm_reass, Params),
	UserData = proplists:get_value(user_data, Params),
	UserDataLen = byte_size(UserData),
	<<?SCCP_MSGT_DT1:8, DstLocalRef:24/big, SegmReass:8, 1:8, UserDataLen:8, UserData/binary>>;
encode_sccp_msgt(?SCCP_MSGT_DT2, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	SeqSegm = proplists:get_value(seq_segm, Params),
	UserData = proplists:get_value(user_data, Params),
	UserDataLen = byte_size(UserData),
	<<?SCCP_MSGT_DT2:8, DstLocalRef:24/big, SeqSegm:16, 1:8, UserDataLen:8, UserData/binary>>;
encode_sccp_msgt(?SCCP_MSGT_AK, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	RxSeqnr = proplists:get_value(rx_seqnr, Params),
	Credit = proplists:get_value(credit, Params),
	<<?SCCP_MSGT_AK:8, DstLocalRef:24/big, RxSeqnr:8, Credit:8>>;
encode_sccp_msgt(?SCCP_MSGT_UDT, Params) ->
	ProtoClass = proplists:get_value(protocol_class, Params),
	CalledParty = proplists:get_value(called_party_addr, Params),
	CalledPartyEnc = encode_sccp_addr(CalledParty),
	CalledPartyLen = byte_size(CalledPartyEnc),
	CallingParty = proplists:get_value(calling_party_addr, Params),
	CallingPartyEnc = encode_sccp_addr(CallingParty),
	CallingPartyLen = byte_size(CallingPartyEnc),
	UserData = proplists:get_value(user_data, Params),
	UserDataLen = byte_size(UserData),
	% variable part
	CalledPartyPtr = 3,
	CallingPartyPtr = 2 + (1 + CalledPartyLen),
	DataPtr = 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
	Remain = <<CalledPartyLen:8, CalledPartyEnc/binary,
		   CallingPartyLen:8, CallingPartyEnc/binary,
		   UserDataLen:8, UserData/binary>>,
	<<?SCCP_MSGT_UDT:8, ProtoClass:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary>>;
encode_sccp_msgt(?SCCP_MSGT_UDTS, Params) ->
	ReturnCause = proplists:get_value(return_cause, Params),
	CalledParty = proplists:get_value(called_party_addr, Params),
	CalledPartyEnc = encode_sccp_addr(CalledParty),
	CalledPartyLen = byte_size(CalledPartyEnc),
	CallingParty = proplists:get_value(calling_party_addr, Params),
	CallingPartyEnc = encode_sccp_addr(CallingParty),
	CallingPartyLen = byte_size(CallingPartyEnc),
	UserData = proplists:get_value(user_data, Params),
	UserDataLen = byte_size(UserData),
	% variable part
	CalledPartyPtr = 3,
	CallingPartyPtr = 2 + (1 + CalledPartyLen),
	DataPtr = 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
	Remain = <<CalledPartyLen:8, CalledPartyEnc/binary,
		   CallingPartyLen:8, CallingPartyEnc/binary,
		   UserDataLen:8, UserData/binary>>,
	<<?SCCP_MSGT_UDTS:8, ReturnCause:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary>>;
encode_sccp_msgt(?SCCP_MSGT_ED, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	UserData = proplists:get_value(user_data, Params),
	UserDataLen = byte_size(UserData),
	DataPtr = 1,
	<<?SCCP_MSGT_ED:8, DstLocalRef:24/big, DataPtr:8, UserDataLen:8, UserData/binary>>;
encode_sccp_msgt(?SCCP_MSGT_EA, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	<<?SCCP_MSGT_EA:8, DstLocalRef:24/big>>;
encode_sccp_msgt(?SCCP_MSGT_RSR, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	ResetCause = proplists:get_value(reset_cause, Params),
	<<?SCCP_MSGT_RSR:8, DstLocalRef:24/big, SrcLocalRef:24/big, ResetCause:8>>;
encode_sccp_msgt(?SCCP_MSGT_RSC, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	<<?SCCP_MSGT_RSC:8, DstLocalRef:24/big, SrcLocalRef:24/big>>;
encode_sccp_msgt(?SCCP_MSGT_ERR, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	ErrCause = proplists:get_value(error_cause, Params),
	<<?SCCP_MSGT_ERR:8, DstLocalRef:24/big, ErrCause:8>>;
encode_sccp_msgt(?SCCP_MSGT_IT, Params) ->
	DstLocalRef = proplists:get_value(dst_local_ref, Params),
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	ProtoClass = proplists:get_value(protocol_class, Params),
	SegmSeq = proplists:get_value(seq_segm, Params),
	Credit = proplists:get_value(credit, Params),
	<<?SCCP_MSGT_IT:8, DstLocalRef:24/big, SrcLocalRef:24/big, ProtoClass:8, SegmSeq:16, Credit:8>>.
% FIXME: XUDT/XUDTS, LUDT/LUDTS


% encode one sccp message data structure into the on-wire format
encode_sccp_msg(#sccp_msg{msg_type = MsgType, parameters = Params}) ->
	encode_sccp_msgt(MsgType, Params).
