-module(osmo_util_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

dl2int_test() ->
	?assertEqual(9876543210, osmo_util:digit_list2int([9,8,7,6,5,4,3,2,1,0])).
int2dl_test() ->
	?assertEqual([9,8,7,6,5,4,3,2,1,0], osmo_util:int2digit_list(9876543210)).

% FIXME: tuple walker test
