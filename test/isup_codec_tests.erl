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

-define(ISUP_IAM_BIN, <<9,0,1,16,72,0,10,3,2,10,8,131,16,41,153,36,0,128,15,10,8,3,19,148,3,
			66,48,147,32,242,21,54,25,8,0,0,21,255,255,255,255,255,255,255,255,
			255,255,29,69,56,203,32,0>>).

-define(ISUP_IAM_DEC, {isup_msg,1,9,[{conn_ind_nature,16},{fw_call_ind,18432},{calling_cat,10},{transm_medium_req,3},{4,{party_number,3,0,undefined,1,undefined,undefined,[9,2,9,9,4,2,0,0,0,8,15]}},{10,{party_number,3,undefined,0,1,0,3,[4,9,3,0,2,4,0,3,3,9,0,2]}},{242,{21,<<54,25,8,0,0,21,255,255,255,255,255,255,255,255,255,255,29,69,56,203,32>>}}]}).

iam_dec_test() ->
	?assertEqual(?ISUP_IAM_DEC, isup_codec:parse_isup_msg(?ISUP_IAM_BIN)).
iam_enc_test() ->
	?assertEqual(?ISUP_IAM_BIN, isup_codec:encode_isup_msg(?ISUP_IAM_DEC)).
