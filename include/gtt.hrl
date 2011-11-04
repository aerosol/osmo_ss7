
-type digit()		:: 0..9.
-type global_title()	:: non_neg_integer().
-type digit_list()	:: [digit()].
-type nature_of_addr()	:: 1..256.	% FIXME
-type numbering_plan()	:: 1..256.	% FIXME

% Record describing a GTT match
-record(gtt_match, {
	 gt_range_from	:: global_title(),	% integer(), GT range lower boundary, included
	 gt_range_to	:: global_title(),	% integer(), GT range upper boundary, included
	 numbering_plan :: numbering_plan(),
	 nature_of_addr_ind :: nature_of_addr(),
	 dpc		:: non_neg_integer(),
	 ssn		:: non_neg_integer()
 	}).

% GTT action for replacing some digits
-record(gtt_act_repl_digits, {
	 replace_digit_start	:: non_neg_integer(),	% digit from which we should replace
	 replace_digit_end	:: non_neg_integer(),
	 new_digits		:: digit_list()
	}).

% GTT action for replacing the numbering plan
-record(gtt_act_repl_num_plan, {
	numbering_plan		:: numbering_plan()
	}).

% GTT action for a generic apply/3 call
-record(gtt_act_apply, {
	 funct,
	 args			:: list()
	}).
