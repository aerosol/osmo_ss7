-module(osmo_ss7_gtt_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").
-include("sccp.hrl").
-include("gtt.hrl").

match_inrange_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1]},
	Match = #gtt_match{gt_range_from = 12340000, gt_range_to = 12350000},
	?assertEqual(foobar, osmo_ss7_gtt:global_title_match([{Match, foobar}], Gt)).

nomatch_outrange_test() ->
	Gt = #global_title{phone_number = [1,2,3,5,0,0,0,1]},
	Match = #gtt_match{gt_range_from = 12340000, gt_range_to = 12350000},
	?assertEqual(false, osmo_ss7_gtt:global_title_match([{Match, foobar}], Gt)).

nomatch_inrange_othercrit_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1]},
	Match = #gtt_match{gt_range_from = 12340000, gt_range_to = 12350000, nature_of_addr_ind = 4},
	?assertEqual(false, osmo_ss7_gtt:global_title_match([{Match, foobar}], Gt)).

match_inrange_othercrit_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1], nature_of_addr_ind = 4},
	Match = #gtt_match{gt_range_from = 12340000, gt_range_to = 12350000, nature_of_addr_ind = 4},
	?assertEqual(foobar, osmo_ss7_gtt:global_title_match([{Match, foobar}], Gt)).


repl_digit_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1], nature_of_addr_ind = 4},
	Act = #gtt_act_repl_digits{replace_digit_start = 1, replace_digit_end = 4, new_digits = [5,6,7,8]},
	ReplGt = osmo_ss7_gtt:apply_gtt_actions(Gt, Act),
	?assertEqual(ReplGt, Gt#global_title{phone_number = [5,6,7,8,0,0,0,1]}).

repl_numplan_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1], numbering_plan = 4},
	Act = #gtt_act_repl_num_plan{numbering_plan = 3},
	ReplGt = osmo_ss7_gtt:apply_gtt_actions(Gt, Act),
	?assertEqual(ReplGt, Gt#global_title{numbering_plan = 3}).

apply_cb(Arg) ->
	Arg.

apply_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1], numbering_plan = 4},
	Act = #gtt_act_apply{funct = fun apply_cb/1, args = [rtfm]},
	ReplGt = osmo_ss7_gtt:apply_gtt_actions(Gt, Act),
	?assertEqual(rtfm, ReplGt).

actlist_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1], numbering_plan = 4},
	Act1 = #gtt_act_repl_digits{replace_digit_start = 1, replace_digit_end = 4, new_digits = [5,6,7,8]},
	Act2 = #gtt_act_repl_num_plan{numbering_plan = 3},
	ReplGt = osmo_ss7_gtt:apply_gtt_actions(Gt, [Act1, Act2]),
	?assertEqual(ReplGt, Gt#global_title{phone_number = [5,6,7,8,0,0,0,1], numbering_plan = 3}).

execute_gtt_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1], numbering_plan = 4},
	Match = #gtt_match{gt_range_from = 12340000, gt_range_to = 12350000},
	Act1 = #gtt_act_repl_digits{replace_digit_start = 1, replace_digit_end = 4, new_digits = [5,6,7,8]},
	Act2 = #gtt_act_repl_num_plan{numbering_plan = 3},
	Rules = [{Match, [Act1, Act2]}],
	ReplGt = osmo_ss7_gtt:execute_gtt(Gt, Rules),
	?assertEqual(ReplGt, Gt#global_title{phone_number = [5,6,7,8,0,0,0,1], numbering_plan = 3}).


execute_gtts_test() ->
	Gt = #global_title{phone_number = [1,2,3,4,0,0,0,1], numbering_plan = 4},
	SccpAddr = #sccp_addr{point_code = 23, ssn = 42, global_title = Gt},
	Match = #gtt_match{gt_range_from = 12340000, gt_range_to = 12350000, dpc = 23},
	% build list of two actions to perform
	Act1 = #gtt_act_repl_digits{replace_digit_start = 1, replace_digit_end = 4, new_digits = [5,6,7,8]},
	Act2 = #gtt_act_repl_num_plan{numbering_plan = 3},
	Rules = [{Match, [Act1, Act2]}],
	% compute the expected result
	ExpGt = Gt#global_title{phone_number = [5,6,7,8,0,0,0,1], numbering_plan = 3},
	ExpSccp = SccpAddr#sccp_addr{global_title = ExpGt},
	ReplSccp = osmo_ss7_gtt:execute_gtt(SccpAddr, Rules),
	?assertEqual(ExpSccp, ReplSccp).
