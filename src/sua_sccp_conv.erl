% Conversion between SUA messages and #sccp_msg{}

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
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

% FIXME: this currently only supports connection-less SCCP

-module(sua_sccp_conv).
-author('Harald Welte <laforge@gnumonks.org>').

-include("sua.hrl").
-include("xua.hrl").
-include("sccp.hrl").

-export([sua_to_sccp/1, sccp_to_sua/1]).

sua_to_sccp(M=#xua_msg{msg_class = Class, msg_type = Type}) ->
	sua_to_sccp(Class, Type, M).
sua_to_sccp(?SUA_MSGC_CL, ?SUA_CL_CLDT, Sua) ->
	Params = sua_to_sccp_params(Sua),
	#sccp_msg{msg_type = ?SCCP_MSGT_UDT,
		parameters = Params};
sua_to_sccp(?SUA_MSGC_CL, ?SUA_CL_CLDR, Sua) ->
	Params = sua_to_sccp_params(Sua),
	#sccp_msg{msg_type = ?SCCP_MSGT_UDTS,
		parameters = Params}.

sccp_to_sua(#sccp_msg{msg_type = Type, parameters = Params}) ->
	sccp_to_sua(Type, Params).
sccp_to_sua(Type, Params) when	Type == ?SCCP_MSGT_UDT;
				Type == ?SCCP_MSGT_XUDT;
				Type == ?SCCP_MSGT_LUDT ->
	Opts = sccp_to_sua_params(Type, Params),
	#xua_msg{version = 1, msg_class = ?SUA_MSGC_CL,
		 msg_type = ?SUA_CL_CLDT, payload = Opts};
sccp_to_sua(Type, Params) when 	Type == ?SCCP_MSGT_UDTS;
				Type == ?SCCP_MSGT_XUDTS;
				Type == ?SCCP_MSGT_LUDTS ->
	Opts = sccp_to_sua_params(Params),
	#xua_msg{version=1, msg_class = ?SUA_MSGC_CL,
		 msg_type = ?SUA_CL_CLDR, payload = Opts}.


% CLDT parameters:
% 	?SUA_IEI_ROUTE_CTX, ?SUA_IEI_PROTO_CLASS, ?SUA_IEI_SRC_ADDR,
% 	?SUA_IEI_DEST_ADDR, ?SUA_IEI_SEQ_CTRL, ?SUA_IEI_S7_HOP_CTR,
% 	?SUA_IEI_IMPORTANCE, ?SUA_IEI_MSG_PRIO, ?SUA_IEI_CORR_ID,
% 	?SUA_IEI_SEGMENTATION, ?SUA_IEI_DATA

sua_to_sccp_params(#xua_msg{msg_class=Class, msg_type=Type, payload=Payload}) ->
	sua_to_sccp_params(Class, Type, Payload).
sua_to_sccp_params(Class, Type, Payload) ->
	sua_to_sccp_params(Class, Type, Payload, []).
sua_to_sccp_params(_Class, _Type, [], List) ->
	List;
sua_to_sccp_params(Class, Type, [{ParTag, {_Len, ParVal}}|Remain], List) ->
	NewPars = sua_to_sccp_param(Class, Type, ParTag, ParVal),
	sua_to_sccp_params(Class, Type, Remain, List ++ NewPars).

% convert an individual SUA parameter to a SCCP option
sua_to_sccp_param(_, _, ?SUA_IEI_PROTO_CLASS, Remain) ->
	<<_:24, PCOpt:4, _:2, Class:2>> = Remain,
	[{protocol_class, {Class, PCOpt}}];
sua_to_sccp_param(_, _, ?SUA_IEI_SRC_ADDR, Remain) ->
	Addr = sua_to_sccp_addr(Remain),
	[{calling_party_addr, Addr}];
sua_to_sccp_param(_, _, ?SUA_IEI_DEST_ADDR, Remain) ->
	Addr = sua_to_sccp_addr(Remain),
	[{called_party_addr, Addr}];
sua_to_sccp_param(_, _, ?SUA_IEI_SEQ_CTRL, _Remain) ->
	% If we were to translate to a N-UNITDATA.req, we could map
	% this, but there is no mapping to a SCCP message...
	[];
sua_to_sccp_param(_, _, ?SUA_IEI_S7_HOP_CTR, Remain) ->
	<<_:24, HopCtr:8>> = Remain,
	[{?SCCP_PNC_HOP_COUNTER, HopCtr}];
sua_to_sccp_param(_, _, ?SUA_IEI_IMPORTANCE, Remain) ->
	<<_:24, Imp:8>> = Remain,
	[{?SCCP_PNC_IMPORTANCE, Imp}];
sua_to_sccp_param(_, _, ?SUA_IEI_DATA, Remain) ->
	[{user_data, Remain}];
sua_to_sccp_param(_, _, ?SUA_IEI_ROUTE_CTX, _Remain) ->
	%FIXME: what to do with routing context?
	[].

sccp_to_sua_params(#sccp_msg{msg_type=Type, parameters=Params}) ->
	sccp_to_sua_params(Type, Params).
sccp_to_sua_params(Type, Params) when is_list(Params) ->
	sccp_to_sua_params(Type, Params, []).
sccp_to_sua_params(_Type, [], List) ->
	List;
sccp_to_sua_params(Type, [{ParTag, ParVal}|Tail], List) ->
	NewPars = sccp_to_sua_param(Type, ParTag, ParVal),
	sccp_to_sua_params(Type, Tail, List ++ NewPars).

sccp_to_sua_param(_, protocol_class, {Opt, Class}) ->
	[{?SUA_IEI_PROTO_CLASS, <<0:24, Opt:4, 0:2, Class:2>>}];
sccp_to_sua_param(_, calling_party_addr, Addr) ->
	AddrSua = sccp_to_sua_addr(Addr),
	[{?SUA_IEI_SRC_ADDR, AddrSua}];
sccp_to_sua_param(_, called_party_addr, Addr) ->
	AddrSua = sccp_to_sua_addr(Addr),
	[{?SUA_IEI_DEST_ADDR, AddrSua}];
sccp_to_sua_param(_, ?SCCP_PNC_HOP_COUNTER, Hop) ->
	[{?SUA_IEI_S7_HOP_CTR, <<0:24, Hop:8>>}];
sccp_to_sua_param(_, ?SCCP_PNC_IMPORTANCE, Imp) ->
	[{?SUA_IEI_IMPORTANCE, <<0:24, Imp:8>>}];
sccp_to_sua_param(_, user_data, Data) ->
	[{?SUA_IEI_DATA, Data}].

sua_to_sccp_addr(SuaBin) ->
	<<RoutInd:16, _:13, GTinc:1, PCinc:1, SSNinc:1, Remain/binary>> = SuaBin,
	ParList = addr_pars_to_list(Remain),
	case GTinc of
		1 ->
			{_, GTopt} = proplists:get_value(?SUA_IEI_GT, ParList),
			GT = parse_sua_gt(GTopt);
		0 ->
			GT = undefined
	end,
	case PCinc of
		1 ->
			{_, PCopt} = proplists:get_value(?SUA_IEI_PC, ParList),
			PC = parse_sua_pc(PCopt);
		0 ->
			PC = undefined
	end,
	case SSNinc of
		1 ->
			{_, SSNopt} = proplists:get_value(?SUA_IEI_SSN, ParList),
			SSN = parse_sua_ssn(SSNopt);
		0 ->
			SSN = undefined
	end,
	case RoutInd of
		?SUA_RI_GT ->
			RoutSSN = 0;
		?SUA_RI_SSN_PC ->
			RoutSSN = 1
	end,
	#sccp_addr{route_on_ssn = RoutSSN, point_code = PC, ssn = SSN, global_title = GT}.

addr_pars_to_list(Bin) ->
	xua_codec:parse_xua_opts(Bin).

sccp_to_sua_addr(Addr) when is_record(Addr, sccp_addr) ->
	#sccp_addr{route_on_ssn = RoutOnSsn, point_code = PC, ssn = SSN,
		   global_title = GT} = Addr,
	case GT of
		#global_title{} ->
			GTopt = [{?SUA_IEI_GT, encode_sua_gt(GT)}],
			GTinc = 1;
		_ ->
			GTopt = [],
			GTinc = 0
	end,
	case PC of
		Int when is_integer(Int) ->
			PCopt = [{?SUA_IEI_PC, encode_sua_pc(PC)}],
			PCinc = 1;
		_ ->
			PCopt = [],
			PCinc = 0
	end,
	case SSN of
		Int2 when is_integer(Int2) ->
			SSNopt = [{?SUA_IEI_SSN, encode_sua_ssn(SSN)}],
			SSNinc = 1;
		_ ->
			SSNopt = [],
			SSNinc = 0
	end,
	case RoutOnSsn of
		0 ->
			RoutInd = ?SUA_RI_GT;
		1 ->
			RoutInd = ?SUA_RI_SSN_PC
	end,
	Tail = xua_codec:encode_xua_opts(GTopt ++ PCopt ++ SSNopt),
	<<RoutInd:16, 0:13, GTinc:1, PCinc:1, SSNinc:1, Tail/binary>>.

parse_sua_gt(Bin) ->
	<<_:24, GTI:8, NoDigits:8, TransType:8, NumPlan:8, NAI:8, Remain/binary>> = Bin,
	Number = parse_sua_gt_digits(NoDigits, Remain),
	#global_title{gti = GTI, nature_of_addr_ind = NAI,
		      trans_type = TransType,
		      numbering_plan = NumPlan,
		      phone_number = Number}.
encode_sua_gt(Gt) when is_record(Gt, global_title) ->
	#global_title{gti = GTI, nature_of_addr_ind = NAI,
		      trans_type = TransType,
		      numbering_plan = NumPlan,
		      phone_number = Number} = Gt,
	NoDigits = count_digits(Number),
	DigitBin = encode_sua_gt_digits(Number),
	<<0:24, GTI:8, NoDigits:8, TransType:8, NumPlan:8, NAI:8, DigitBin/binary>>.

count_digits(Number) when is_integer(Number) ->
	BcdList = osmo_util:int2digit_list(Number),
	count_digits(BcdList);
count_digits(Number) when is_list(Number) ->
	length(Number).


parse_sua_gt_digits(NoDigits, Remain) ->
	% as opposed to ISUP/SCCP, we can have more than one nibble padding,
	OddEven = NoDigits rem 1,
	case OddEven of
		0 ->
			ByteLen = NoDigits div 2;
		1 ->
			ByteLen = NoDigits div 2 + 1
	end,
	<<Bin:ByteLen/binary, _/binary>> = Remain,
	isup_codec:parse_isup_party(Bin, OddEven).
encode_sua_gt_digits(Digits) when is_list(Digits); is_integer(Digits) ->
	% Assume that overall option encoder will do the padding...
	{Enc, _OddEven} = isup_codec:encode_isup_party(Digits),
	Enc.

parse_sua_pc(<<PC:32/big>>) ->
	PC.
encode_sua_pc(Pc) when is_integer(Pc) ->
	<<Pc:32/big>>.

parse_sua_ssn(<<_:24, SSN:8>>) ->
	SSN.
encode_sua_ssn(Ssn) when is_integer(Ssn) ->
	<<0:24, Ssn:8>>.
