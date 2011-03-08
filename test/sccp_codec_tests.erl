-module(sccp_codec_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include("sccp.hrl").

-define(SCCP_MSG_BIN, <<9,0,3,13,24,10,18,7,0,18,4,83,132,9,0,23,11,18,6,0,18,4,68,119,88,16,70,35,67,100,65,73,4,81,1,2,200,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,0,108,13,163,11,2,1,64,2,1,8,48,3,10,1,0>>).

-define(SCCP_MSG_DEC, {sccp_msg,9,[{protocol_class,0},{called_party_addr,{sccp_addr,0,0,undef,7,{global_title,4,4,0,2,1,[3,5,4,8,9,0,0,0,7,1]}}},{calling_party_addr,{sccp_addr,0,0,undef,6,{global_title,4,4,0,2,1,[4,4,7,7,8,5,0,1,6,4,3,2]}}},{user_data,<<100,65,73,4,81,1,2,200,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,0,108,13,163,11,2,1,64,2,1,8,48,3,10,1,0>>}]}).

parse_test() ->
	?assertEqual({ok, ?SCCP_MSG_DEC}, sccp_codec:parse_sccp_msg(?SCCP_MSG_BIN)).
encode_test() ->
	?assertEqual(?SCCP_MSG_BIN, sccp_codec:encode_sccp_msg(?SCCP_MSG_DEC)).
