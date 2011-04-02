% Osmocom Global Title Translation

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

-module(osmo_ss7_gtt).
-author('Harald Welte <laforge@gnumonks.org>').

-include("sccp.hrl").
-include("gtt.hrl").

-export([global_title_match/2, apply_gtt_actions/2, execute_gtt/2]).

-compile({parse_transform, exprecs}).
-export_records([gtt_match, gtt_act_repl_digits, gtt_act_repl_num_plan]).

% Match a given GT against an ordered list of {match, action} tuples
global_title_match([], _Gt) ->
	false;
global_title_match([{Match, Action}|Tail], Gt) when is_record(Gt, global_title) ->
	PhoneNumInt = osmo_util:digit_list2int(Gt#global_title.phone_number),
	if Match#gtt_match.gt_range_from >= PhoneNumInt ->
		% in an ordered list, we can assume that no trailing rules will match
		false;
	   true ->
		case single_gt_match(Match, Gt) of
		    true ->
			Action;
		    _ ->
			% iterate further over the list of GTT rules
			global_title_match(Tail, Gt)
		end
	end;
% Same as above, but for SCCP Address (i.e. GT + point code and SSN)
global_title_match([{Match, Action}|Tail], SccpAddr) when is_record(SccpAddr, sccp_addr) ->
	Gt = SccpAddr#sccp_addr.global_title,
	PhoneNumInt = osmo_util:digit_list2int(Gt#global_title.phone_number),
	if Match#gtt_match.gt_range_from >= PhoneNumInt ->
		% in an ordered list, we can assume that no trailing rules will match
		false;
	   true ->
		case single_gt_match(Match, SccpAddr) of
		    true ->
			Action;
		    _ ->
			% iterate further over the list of GTT rules
			global_title_match(Tail, SccpAddr)
		end
	end.


% perform matching of a given global title against a single match
single_gt_match(Match, Gt) when is_record(Match, gtt_match), is_record(Gt, global_title) ->
	#gtt_match{gt_range_from = RangeFrom, gt_range_to = RangeTo,
		   numbering_plan = NumPlan, nature_of_addr_ind = NatureInd} = Match,
	% build a list of the individual criteria that all have to match
	SubMatchList = [{digits, {RangeFrom, RangeTo}, Gt#global_title.phone_number},
			{numbering_plan, NumPlan, Gt#global_title.numbering_plan},
			{nature_of_addr_ind, NatureInd, Gt#global_title.nature_of_addr_ind}],
	gt_sub_match_list(SubMatchList);
% Same as above, but for SCCP Address (i.e. GT + point code and SSN)
single_gt_match(Match, SccpAddr) when is_record(Match, gtt_match), is_record(SccpAddr, sccp_addr) ->
	#gtt_match{dpc = Dpc, ssn = Ssn} = Match,
	Gt = SccpAddr#sccp_addr.global_title,
	% First match the GT part
	case single_gt_match(Match, Gt) of
	    false ->
		false;
	    true ->
		% build a list of the individual criteria that all have to match
		SubMatchList = [{dpc, Dpc}, {ssn, Ssn}],
		gt_sub_match_list(SubMatchList)
	end.

% iterate over the list of individual match criteria and call the match function
gt_sub_match_list([]) ->
	true;
gt_sub_match_list([{What, MatchPart, GtPart}|SubMatchList]) ->
	case gt_sub_match(What, MatchPart, GtPart) of
		false ->
			false;
		true ->
			gt_sub_match_list(SubMatchList)
	end.

% matching of the actual phone number digits
gt_sub_match(digits, {DigitsFrom, DigitsTo}, GtPart) ->
	PhoneNumInt = osmo_util:digit_list2int(GtPart),
	if
		PhoneNumInt >= DigitsFrom, PhoneNumInt =< DigitsTo -> true;
		true -> false
	end;
% any match that is not qualified will always match
gt_sub_match(_What, undefined, _GtPart) ->
	true;
% remaining default match for all other fields
gt_sub_match(_What, MatchPart, GtPart) ->
	if
		MatchPart == GtPart -> true;
		true -> false
	end.


% Execute a single action: Replac some digits in the GT
gtt_action(Gt, Action) when is_record(Gt, global_title), is_record(Action, gtt_act_repl_digits) ->
	#gtt_act_repl_digits{replace_digit_start = ReplDigStart,
			   replace_digit_end = ReplDigEnd,
			   new_digits = NewDigits} = Action,
	GtDigitList = Gt#global_title.phone_number,
	Header = lists:sublist(GtDigitList, 1, ReplDigStart-1),
	Trailer = lists:sublist(GtDigitList, ReplDigEnd+1, length(GtDigitList)),
	Gt#global_title{phone_number = Header ++ NewDigits ++ Trailer};

% Execute a single action: Replac the numbering plan in the GT
gtt_action(Gt,Action) when is_record(Gt, global_title), is_record(Action, gtt_act_repl_num_plan) ->
	NewNumPlan = Action#gtt_act_repl_num_plan.numbering_plan,
	Gt#global_title{numbering_plan = NewNumPlan}.

% appliy a list of GTT actions to a Global Title
apply_gtt_actions(Gt, []) when is_record(Gt, global_title) ->
	Gt;
apply_gtt_actions(Gt, [Head|List]) when is_record(Gt, global_title) ->
	NewGt = gtt_action(Gt, Head),
	apply_gtt_actions(NewGt, List);
apply_gtt_actions(Gt, Action) when is_record(Gt, global_title) ->
	gtt_action(Gt, Action).

% Execute a complete GTT operation: matching + executing the action
execute_gtt(Gt, RulesList) when is_record(Gt, global_title), is_list(RulesList) ->
	case global_title_match(RulesList, Gt) of
	    false ->
		Gt;
	    Action ->
		apply_gtt_actions(Gt, Action)
	end;
% Same as above, but for SCCP Address (i.e. GT + point code and SSN)
execute_gtt(SccpAddr, RulesList) when is_record(SccpAddr, sccp_addr), is_list(RulesList) ->
	Gt = SccpAddr#sccp_addr.global_title,
	NewGt = execute_gtt(Gt, RulesList),
	SccpAddr#sccp_addr{global_title = NewGt}.
