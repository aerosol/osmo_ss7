-module(linksets).
-author('adam.rutkowski@jtendo.com').

-behaviour(confetti_client).
%-behaviour(gen_event).
-export[start/0, process_config_in/1, process_config_out/2].
-export[init/1, handle_event/2].

-include_lib("kernel/include/inet_sctp.hrl").

init([]) ->
    {ok, []}.

process_config_in(Conf) ->
    {ok, Conf}.

process_config_out(RawConf, _Conf) ->
    Commented = "%% This is automatic dump\n" ++ binary_to_list(RawConf),
    {ok, list_to_binary(Commented)}.

handle_event(Event, State) ->
    io:format("Handling event ~p~n", [Event]),
    {ok, State}.

start() ->
    confetti_client:start(linksets, "conf", "linksets.conf", ?MODULE).







