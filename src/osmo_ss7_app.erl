-module(osmo_ss7_app).
-behaviour(application).
-author('Harald Welte <laforge@gnumonks.org>').

% application behaviour callbacks
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

-export([reload_config/0]).

start(normal, StartArgs) ->
	supervisor:start_link({local, osmo_ss7_sup}, osmo_ss7_sup, StartArgs).


start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

prep_stop(State) ->
	State.

stop(_State) ->
	ok.

config_change(_Changed, _New, _Removed) ->
	ok.



reload_config() ->
	osmo_util:reload_config(),
	% FIXME: do something
	ok.
