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

parse_isup_party(<<>>, OddEven, DigitList) ->
	% in case of odd number of digits, we need to cut the last
	case OddEven of
		1 ->
			lists:sublist(DigitList, lists:length(DigitList)-1);
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
parse_isup_opt(?ISUP_PAR_CALLED_P_NUM, OptLen, Content) ->
	% C.3.7 Called Party Number
	<<OddEven:1, Nature:7, Inn:1, NumPlan:3, 0:4, Remain/binary>> = Content,
	PhoneNum = parse_isup_party(Remain, OddEven),
	{OptType, #party_number{nature_of_addr_ind = Nature,
				internal_net_num = Inn,
				numbering_plan = NumPlan,
				phone_number = PhoneNum}};
parse_isup_opt(?ISUP_PAR_CALLING_P_NUM, OptLen, Content) ->
	% C.3.8 Calling Party Number
	<<OddEven:1, Nature:7, Ni:1, NumPlan:3, PresRestr:2, Screen:2, Remain/binary>> = Content,
	PhoneNum = parse_isup_party(Remain, OddEven),
	{OptType, #party_number{nature_of_addr_ind = Nature,
				number_incompl_ind = Ni,
				numbering_plan = NumPlan,
				present_restrict = PresRestr,
				screening_ind = Screen,
				phone_number = PhoneNum}};
parse_isup_opt(?ISUP_PAR_CONNECTED_NUM, OptLen, Content) ->
	% C.3.14 Connected Number
	<<OddEven:1, Nature:7, 0:1, NumPlan:3, PresRestr:2, Screen:2, Remain/binary>> = Content,
	PhoneNum = parse_isup_party(Remain, OddEven),
	{OptType, #party_number{nature_of_addr_ind = Nature,
				numbering_plan = NumPlan,
				present_restrict = PresRestr,
				screening_ind = Screen,
				phone_number = PhoneNum}};
parse_isup_opt(?ISUP_PAR_SUBSEQ_NUM, OptLen, Content) ->
	% C.3.32 Subsequent Number
	<<OddEven:1, Spare:7, Remain/binary>> = Content,
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
	parse_isup_opts(Remain, [NewOpt|OptList]).

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
	0;
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
	FixedOpts = [{conn_ind_nature, CINat}, {fw_call_ind, FwCallInd}, {calling_cat, CallingCat},
		     {transm_medium_req, TransmReq}],
	% V: Called Party Number
	VarOpts = FIXME,
	Opts = parse_isup_opts(Remain),
	[FixedOpts,VarOpts,Opts];
% Table C-17	Release
parse_isup_msgt(?ISUP_MSGT_REL, Bin) ->
	% V: Cause indicators
	VarOpts = FIXME,
	Opts = parse_isup_opts(Remain),
	[VarOpts,Opts];
% Table C-19	Subsequent address
parse_isup_msgt(?ISUP_MSGT_SAM, Bin) ->
	% V: Subsequent number
	VarOpts = FIXME,
	Opts = parse_isup_opts(Remain),
	[VarOpts,Opts];
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
		[].
% Table C-25
parse_isup_msgt(M, Bin) when
	M == ?ISUP_MSGT_CGB;
	M == ?ISUP_MSGT_CGBA;
	M == ISUP_MSGT_CGU;
	M == ISUP_MSGT_CGUA ->
		<<CGMsgt:8, VarBin/binary>> = Bin,
		FixedOpts = [{cg_supv_msgt, CGMsgt}],
		% V: Range and status
		VarOpts = FIXME,
		[FixedOpts|VarOpts];
% Table C-26	Circuit group reset
parse_isup_msgt(?ISUP_MSGT_GRS, Bin) ->
	% V: Range without status
	VarOpts = FIXME,
	VarOpts.


parse_isup_msg(Databin) when is_binary(DataBin) ->
	<<0:4, Cic:12/big, MsgType:8, Remain/binary>> = DataBin,
	Opts = parse_isup_msgt(MsgType, Remain),
	#isup_msg{cic = Cic, msg_type = MsgType, parameters = Opts}.


encode_isup_party(BcdList) ->
	encode_isup_party(BcdList, <<>>, list:length(BcdList)).
encode_isup_party([], Bin, NumDigits) ->
	case NumDigits rem 2 of
		1 ->
			{Bin, 1};
		0 ->
			{Bin, 0}
	end;
encode_isup_party([First,Second|BcdList], Bin, NumDigits) ->
	encode_isup_party(BcdList, <<Bin/binary, Second:4, First:4>>).
	
% encode a single option
encode_isup_opt(?ISUP_PAR_CALLED_P_NUM,
		#party_number{nature_of_addr_ind = Nature,
			      internal_net_num = Inn,
			      numbering_plan = NumPlan,
			      phone_number=  PhoneNum}) ->
	% C.3.7 Called Party Number
	{PhoneBin, OddEven} = encode_isup_party(PhoneNum),
	<<OddEven:1, Nature:7, Inn:1, NumPlan:3, 0:4, PhoneBin/binary>>.
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

