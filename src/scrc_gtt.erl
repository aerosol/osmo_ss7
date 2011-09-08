-module(scrc_gtt).
-author('adam.rutkowski@jtendo.com').

-export([needs_translation/1]).

-include("sccp.hrl").

needs_translation(SccpAddr = #sccp_addr{})
    when SccpAddr#sccp_addr.global_title =/= undefined ->
        io:format("This address needs translation~n"),
        true;

needs_translation(SccpAddr) ->
    io:format("Does not need translation.~n"),
    false.


