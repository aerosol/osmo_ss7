-module(isup_codec_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include("isup.hrl").


-define(ISUP_GRS_BIN, <<1,0,23,1,1,14>>).
-define(ISUP_GRS_DEC, #isup_msg{msg_type = 23,cic = 1, parameters = [{22,{1,<<14>>}}]}).

grs_dec_test() ->
	?assertEqual(?ISUP_GRS_DEC, isup_codec:parse_isup_msg(?ISUP_GRS_BIN)).
grs_enc_test() ->
	?assertEqual(?ISUP_GRS_BIN, isup_codec:encode_isup_msg(?ISUP_GRS_DEC)).

