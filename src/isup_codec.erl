% ITU-T Q.76x ISUPcoding / decoding

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

-module(isup_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("isup.hrl").

-export([parse_isup_msg/1, encode_isup_msg/1]).

-compile(export_all).

parse_isup_party(<<>>, OddEven, DigitList) ->
	% in case of odd number of digits, we need to cut the last
	case OddEven of
		1 ->
			lists:sublist(DigitList, length(DigitList)-1);
		0 ->
			DigitList
	end;
parse_isup_party(BcdBin, OddEven, DigitList) ->
	<<Second:4, First:4, Remain/binary>> = BcdBin,
	NewDigits = [First, Second],
	parse_isup_party(Remain, OddEven, DigitList ++ NewDigits).

parse_isup_party(BinBcd, OddEven) when is_binary(BinBcd) ->
	parse_isup_party(BinBcd, OddEven, []).


% parse a single option
parse_isup_opt(OptType = ?ISUP_PAR_CALLED_P_NUM, _OptLen, Content) ->
	% C.3.7 Called Party Number
	<<OddEven:1, Nature:7, Inn:1, NumPlan:3, 0:4, Remain/binary>> = Content,
	PhoneNum = parse_isup_party(Remain, OddEven),
	{OptType, #party_number{nature_of_addr_ind = Nature,
				internal_net_num = Inn,
				numbering_plan = NumPlan,
				phone_number = PhoneNum}};
parse_isup_opt(OptType = ?ISUP_PAR_CALLING_P_NUM, _OptLen, Content) ->
	% C.3.8 Calling Party Number
	<<OddEven:1, Nature:7, Ni:1, NumPlan:3, PresRestr:2, Screen:2, Remain/binary>> = Content,
	PhoneNum = parse_isup_party(Remain, OddEven),
	{OptType, #party_number{nature_of_addr_ind = Nature,
				number_incompl_ind = Ni,
				numbering_plan = NumPlan,
				present_restrict = PresRestr,
				screening_ind = Screen,
				phone_number = PhoneNum}};
parse_isup_opt(OptType = ?ISUP_PAR_CONNECTED_NUM, _OptLen, Content) ->
	% C.3.14 Connected Number
	<<OddEven:1, Nature:7, 0:1, NumPlan:3, PresRestr:2, Screen:2, Remain/binary>> = Content,
	PhoneNum = parse_isup_party(Remain, OddEven),
	{OptType, #party_number{nature_of_addr_ind = Nature,
				numbering_plan = NumPlan,
				present_restrict = PresRestr,
				screening_ind = Screen,
				phone_number = PhoneNum}};
parse_isup_opt(OptType = ?ISUP_PAR_SUBSEQ_NUM, _OptLen, Content) ->
	% C.3.32 Subsequent Number
	<<OddEven:1, 0:7, Remain/binary>> = Content,
	PhoneNum = parse_isup_party(Remain, OddEven),
	{OptType, #party_number{phone_number = PhoneNum}};
parse_isup_opt(OptType, OptLen, Content) ->
	{OptType, {OptLen, Content}}.

% parse a Binary into a list of options
parse_isup_opts(<<>>, OptList) ->
	% empty list
	OptList;
parse_isup_opts(<<0>>, OptList) ->
	% end of options
	OptList;
parse_isup_opts(OptBin, OptList) when is_binary(OptBin) ->
	<<OptType:8, OptLen:8, Content:OptLen/binary, Remain/binary>> = OptBin,
	NewOpt = parse_isup_opt(OptType, OptLen, Content),
	parse_isup_opts(Remain, OptList ++ [NewOpt]).
parse_isup_opts(OptBin) ->
	parse_isup_opts(OptBin, []).

% References to 'Tabe C-xxx' are to Annex C of Q.767

% Default case: no fixed and no variable parts, only options
% ANM, RLC, FOT
parse_isup_msgt(M, Bin) when
	M == ?ISUP_MSGT_ANM;
	M == ?ISUP_MSGT_RLC;
	M == ?ISUP_MSGT_FOT ->
		parse_isup_opts(Bin);
% Table C-5	Address complete
parse_isup_msgt(?ISUP_MSGT_ACM, Bin) ->
	<<BackCallInd:16, Remain/binary>> = Bin,
	BciOpt = {backward_call_ind, BackCallInd},
	Opts = parse_isup_opts(Remain),
	[BciOpt|Opts];
% Table C-7	Call progress
parse_isup_msgt(?ISUP_MSGT_CPG, Bin) ->
	<<EventInf:8, Remain/binary>> = Bin,
	BciOpt = {event_info, EventInf},
	Opts = parse_isup_opts(Remain),
	[BciOpt|Opts];
% Table C-9	Circuit group reset acknowledgement
parse_isup_msgt(?ISUP_MSGT_GRA, Bin) ->
	% V: Range and status
	<<PtrVar:8, Remain/binary>> = Bin,
	RangStsLen = binary:at(Remain, PtrVar),
	RangeStatus = binary:part(Remain, PtrVar+1, RangStsLen),
	RangeStsTuple = {?ISUP_PAR_RANGE_AND_STATUS, {RangStsLen, RangeStatus}},
	[RangeStsTuple];
% Table C-11	Connect
parse_isup_msgt(?ISUP_MSGT_CON, Bin) ->
	<<BackCallInd:16, Remain/binary>> = Bin,
	BciOpt = {backward_call_ind, BackCallInd},
	Opts = parse_isup_opts(Remain),
	[BciOpt|Opts];
% Table C-12	Continuity
parse_isup_msgt(?ISUP_MSGT_COT, Bin) ->
	<<ContInd:8>> = Bin,
	[{continuity_ind, ContInd}];
% Table C-16	Initial address
parse_isup_msgt(?ISUP_MSGT_IAM, Bin) ->
	<<CINat:8, FwCallInd:16/big, CallingCat:8, TransmReq:8, VarAndOpt/binary>> = Bin,
	%<<CINat:8, FwCallInd:16/big, CallingCat:8, TransmReq:8, PtrVar:8, PtrOpt:8, VarAndOpt/binary>> = Bin,
	FixedOpts = [{conn_ind_nature, CINat}, {fw_call_ind, FwCallInd}, {calling_cat, CallingCat},
		     {transm_medium_req, TransmReq}],
	<<PtrVar:8, PtrOpt:8, _/binary>> = VarAndOpt,
	% V: Called Party Number
	CalledPartyLen = binary:at(VarAndOpt, PtrVar),
	CalledParty = binary:part(VarAndOpt, PtrVar+1, CalledPartyLen),
	VarOpts = [parse_isup_opt(?ISUP_PAR_CALLED_P_NUM, CalledPartyLen, CalledParty)],
	% Optional part
	Remain = binary:part(VarAndOpt, 1 + PtrOpt, byte_size(VarAndOpt)-(1+PtrOpt)),
	Opts = parse_isup_opts(Remain),
	FixedOpts ++ VarOpts ++ Opts;
% Table C-17	Release
parse_isup_msgt(?ISUP_MSGT_REL, Bin) ->
	<<PtrVar:8, PtrOpt:8, VarAndOpt/binary>> = Bin,
	% V: Cause indicators
	CauseIndLen = binary:at(VarAndOpt, PtrVar),
	CauseInd = binary:part(VarAndOpt, PtrVar+1, CauseIndLen),
	VarOpts = {?ISUP_PAR_CAUSE_IND, {CauseIndLen, CauseInd}},
	Remain = binary:part(VarAndOpt, 1 + PtrOpt, byte_size(VarAndOpt)-(1+PtrOpt)),
	Opts = parse_isup_opts(Remain),
	VarOpts ++ Opts;
% Table C-19	Subsequent address
parse_isup_msgt(?ISUP_MSGT_SAM, Bin) ->
	<<PtrVar:8, PtrOpt:8, VarAndOpt/binary>> = Bin,
	% V: Subsequent number
	SubseqNumLen = binary:at(VarAndOpt, PtrVar),
	SubsetNum = binary:part(VarAndOpt, PtrVar+1, SubseqNumLen),
	VarOpts = [{?ISUP_PAR_SUBSEQ_NUM, {SubseqNumLen, SubsetNum}}],
	Remain = binary:part(VarAndOpt, 1 + PtrOpt, byte_size(VarAndOpt)-(1+PtrOpt)),
	Opts = parse_isup_opts(Remain),
	VarOpts ++ Opts;
% Table C-21	Suspend, Resume
parse_isup_msgt(Msgt, Bin) when Msgt == ?ISUP_MSGT_RES; Msgt == ?ISUP_MSGT_SUS ->
	<<SuspResInd:8, Remain/binary>> = Bin,
	FixedOpts = [{susp_res_ind, SuspResInd}],
	Opts = parse_isup_opts(Remain),
	[FixedOpts|Opts];
% Table C-23
parse_isup_msgt(M, <<>>) when
	M == ?ISUP_MSGT_BLO;
	M == ?ISUP_MSGT_BLA;
	M == ?ISUP_MSGT_CCR;
	M == ?ISUP_MSGT_RSC;
	M == ?ISUP_MSGT_UBL;
	M == ?ISUP_MSGT_UBA ->
		[];
% Table C-25
parse_isup_msgt(M, Bin) when
	M == ?ISUP_MSGT_CGB;
	M == ?ISUP_MSGT_CGBA;
	M == ?ISUP_MSGT_CGU;
	M == ?ISUP_MSGT_CGUA ->
		<<CGMsgt:8, PtrVar:8, VarBin/binary>> = Bin,
		FixedOpts = [{cg_supv_msgt, CGMsgt}],
		% V: Range and status
		RangStsLen = binary:at(VarBin, PtrVar),
		RangeStatus = binary:part(VarBin, PtrVar+1, RangStsLen),
		VarOpts = [{?ISUP_PAR_RANGE_AND_STATUS, {RangStsLen, RangeStatus}}],
		FixedOpts ++ VarOpts;
% Table C-26	Circuit group reset
parse_isup_msgt(?ISUP_MSGT_GRS, Bin) ->
	<<PtrVar:8, VarBin/binary>> = Bin,
	% V: Range without status
	RangeLen = binary:at(VarBin, PtrVar),
	Range = binary:part(VarBin, PtrVar+1, RangeLen),
	[{?ISUP_PAR_RANGE_AND_STATUS, {RangeLen, Range}}].


parse_isup_msg(DataBin) when is_binary(DataBin) ->
	<<Cic:12/little, 0:4, MsgType:8, Remain/binary>> = DataBin,
	Opts = parse_isup_msgt(MsgType, Remain),
	#isup_msg{cic = Cic, msg_type = MsgType, parameters = Opts}.


% encode a phone number from a list of digits into the BCD binary sequence
encode_isup_party(BcdList) ->
	encode_isup_party(BcdList, <<>>, length(BcdList)).
encode_isup_party([], Bin, NumDigits) ->
	case NumDigits rem 2 of
		1 ->
			{Bin, 1};
		0 ->
			{Bin, 0}
	end;
encode_isup_party([First,Second|BcdList], Bin, NumDigits) ->
	encode_isup_party(BcdList, <<Bin/binary, Second:4, First:4>>, NumDigits).

% encode a single option
encode_isup_opt(?ISUP_PAR_CALLED_P_NUM,
		#party_number{nature_of_addr_ind = Nature,
			      internal_net_num = Inn,
			      numbering_plan = NumPlan,
			      phone_number=  PhoneNum}) ->
	% C.3.7 Called Party Number
	{PhoneBin, OddEven} = encode_isup_party(PhoneNum),
	<<OddEven:1, Nature:7, Inn:1, NumPlan:3, 0:4, PhoneBin/binary>>;
encode_isup_opt(?ISUP_PAR_CALLING_P_NUM,
		#party_number{nature_of_addr_ind = Nature,
			      number_incompl_ind = Ni,
			      numbering_plan = NumPlan,
			      present_restrict = PresRestr,
			      screening_ind = Screen,
			      phone_number=  PhoneNum}) ->
	% C.3.8 Calling Party Number
	{PhoneBin, OddEven} = encode_isup_party(PhoneNum),
	<<OddEven:1, Nature:7, Ni:1, NumPlan:3, PresRestr:2, Screen:2, PhoneBin/binary>>;
encode_isup_opt(?ISUP_PAR_CONNECTED_NUM,
		#party_number{nature_of_addr_ind = Nature,
			      numbering_plan = NumPlan,
			      present_restrict = PresRestr,
			      screening_ind = Screen,
			      phone_number = PhoneNum}) ->
	% C.3.14 Connected Number
	{PhoneBin, OddEven} = encode_isup_party(PhoneNum),
	<<OddEven:1, Nature:7, 0:1, NumPlan:3, PresRestr:2, Screen:2, PhoneBin/binary>>;
encode_isup_opt(OptNum, {OptLen, Binary}) when is_binary(Binary) ->
	Binary.

encode_isup_msg(#isup_msg{}) ->
	foo.
