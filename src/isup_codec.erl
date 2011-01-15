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

% References to 'Tabe C-xxx' are to Annex C of Q.767

% Default case: no fixed and no variable parts, only options
% ANM, RLC, FOT
parse_isup_msgt(M, Bin) when
	M == ?ISUP_MSGT_ANM;
	M == ?ISUP_MSGT_RLC;
	M == ?ISUP_MSGT_FOT;
		parse_isup_opts(Bin);
% Table C-5	Address complete
parse_isup_msgt(?ISUP_MSGT_ACM, Bin) ->
	<<BackCallInd:16, Remain/binary>> = Bin,
	BciOpt = {backward_call_ind, BackCallInd},
	Opts = parse_isup_opts(Remain)
	[BciOpt|Opts];
% Table C-7	Call progress
parse_isup_msgt(?ISUP_MSGT_CPG, Bin) ->
	<<EventInf:8, Remain/binary>> = Bin,
	BciOpt = {event_info, EventInf},
	Opts = parse_isup_opts(Remain)
	[BciOpt|Opts];
% Table C-9	Circuit group reset acknowledgement
parse_isup_msgt(?ISUP_MSGT_GRA, Bin) ->
	% V: Range and status

% Table C-11	Connect
parse_isup_msgt(?ISUP_MSGT_CON, Bin) ->
	<<BackCallInd:16, Remain/binary>> = Bin,
	BciOpt = {backward_call_ind, BackCallInd},
	Opts = parse_isup_opts(Remain)
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
	VarOpts = FIXME;
	Opts = parse_isup_opts(Remain),
	[FixedOpts,VarOpts,Opts];
% Table C-17	Release
parse_isup_msgt(?ISUP_MSGT_REL, Bin) ->
	% V: Cause indicators
	VarOpts = FIXME;
	Opts = parse_isup_opts(Remain),
	[VarOpts,Opts];
% Table C-19	Subsequent address
parse_isup_msgt(?ISUP_MSGT_SAM, Bin) ->
	% V: Subsequent number
	VarOpts = FIXME;
	Opts = parse_isup_opts(Remain),
	[VarOpts,Opts];
% Table C-21	Suspend, Resume
parse_isup_msgt(Msgt, Bin) when Msgt == ?ISUP_MSGT_RES or Msgt == ?ISUP_MSGT_SUS ->
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
