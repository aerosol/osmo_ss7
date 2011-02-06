-module(mgw_nat_app).
-behavior(application).
-export([start/2, stop/1]).

-export([reload_config/0]).

start(_Type, _Args) ->
	Sup = mgw_nat_sup:start_link(),
	io:format("Sup ~p~n", [Sup]),
	Sup.

stop(_State) ->
	ok.

reload_config() ->
	case init:get_argument(config) of
	{ok, [ Files ]} ->
		ConfFiles = [begin
				S = filename:basename(F,".config"),
				filename:join(filename:dirname(F),
				S ++ ".config")
			     end || F <- Files],
		% Move sys.config to the head of the list
		Config = lists:sort(fun("sys.config", _) -> true;
					(_, _) -> false end, ConfFiles),

		OldEnv = application_controller:prep_config_change(),

		Apps = [{application, A, make_appl(A)}
		|| {A,_,_} <- application:which_applications()],
		application_controller:change_application_data(Apps, Config),
		application_controller:config_change(OldEnv);
	_ ->
		{ok, []}
	end.

make_appl(App) when is_atom(App) ->
	AppList  = element(2,application:get_all_key(App)),
	FullName = code:where_is_file(atom_to_list(App) ++ ".app"),
	case file:consult(FullName) of
	{ok, [{application, _, Opts}]} ->
		Env = proplists:get_value(env, Opts, []),
		lists:keyreplace(env, 1, AppList, {env, Env});
	{error, _Reason} ->
		lists:keyreplace(env, 1, AppList, {env, []})
	end.
