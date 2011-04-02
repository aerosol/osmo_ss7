% Record describing a GTT match
-record(gtt_match, {
	 gt_range_from,		% integer(), GT range lower boundary, included
	 gt_range_to,		% integer(), GT range upper boundary, included
	 numbering_plan,	% integer()
	 nature_of_addr_ind,	% integer()
	 dpc,			% integer()
	 ssn}).

% GTT action for replacing some digits
-record(gtt_act_repl_digits, {
	 replace_digit_start,	% integer(), digit from which we should replace
	 replace_digit_end,	% integer
	 new_digits		% list of integers
	}).

% GTT action for replacing the numbering plan
-record(gtt_act_repl_num_plan, {
	 numbering_plan
	}).

% GTT action for a generic apply/3 call
-record(gtt_act_apply, {
	 funct,
	 args
	}).
