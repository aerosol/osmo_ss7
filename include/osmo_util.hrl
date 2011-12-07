
-type prim_spec_name() :: 'request' | 'response' | 'confirm' | 'indication'.

% a primitive how it is used inside the SCCP stack and to the user
-record(primitive, {
	  subsystem,
	  gen_name,
	  spec_name :: prim_spec_name(),
	  parameters
	}).

-type pointcode_repr() :: 'itu' | 'ansi' | 'ttc'.

-record(pointcode, {
	repr :: pointcode_repr(),
	value
}).
