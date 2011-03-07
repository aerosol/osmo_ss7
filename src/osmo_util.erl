% Osmocom Erlang utility functions

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(osmo_util).
-author('Harald Welte <laforge@gnumonks.org>').

-export([digit_list2int/1, int2digit_list/1]).
-export([reload_config/0]).
-export([tuple_walk/2, tuple_walk_print_cb/2]).

% Convert a list of digits to an integer value
digit_list2int(Int, []) ->
	Int;
digit_list2int(Int, [Digit|Tail]) ->
	digit_list2int(Int*10 + Digit, Tail).
digit_list2int(Digits) when is_list(Digits) ->
	digit_list2int(0, Digits).

% Convert an integer value into a list of decimal digits
int2digit_list(0, Digits) when is_list(Digits) ->
	Digits;
int2digit_list(Int, Digits) when is_integer(Int), is_list(Digits) ->
	Digit = Int rem 10,
	int2digit_list(Int div 10, [Digit|Digits]).
int2digit_list(Int) when is_integer(Int) ->
	int2digit_list(Int, []).

% reload configuration of an application
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


% Walk a named tuple and (recursively) all its fields, call user-supplied
% callback for each of them
tuple_walk(Tpl, TupleCb) when is_tuple(Tpl), is_function(TupleCb) ->
	tuple_walk([], Tpl, TupleCb).

tuple_walk(Path, Tpl, TupleCb) when is_list(Path), is_tuple(Tpl) ->
	% call Callback
	NewTpl = TupleCb(Path, Tpl),
	[TplName|TplList] = tuple_to_list(NewTpl),
	NewTplList = tuple_fieldlist_walk(Path, TplName, TplList, TupleCb),
	list_to_tuple([TplName|NewTplList]);
tuple_walk(Path, TplL, TupleCb) when is_list(Path), is_list(TplL) ->
	tuple_walk_list(Path, TplL, TupleCb, []).

tuple_walk_list(_Path, [], _TupleCb, OutList) ->
	OutList;
tuple_walk_list(Path, [Head|Tail], TupleCb, OutList) ->
	if
		is_tuple(Head) ->
			NewHead = tuple_walk(Path, Head, TupleCb);
		is_list(Head) ->
			NewHead = tuple_walk(Path, Head, TupleCb);
		true ->
			NewHead = Head
	end,
	tuple_walk_list(Path, Tail, TupleCb, OutList++[NewHead]).


tuple_fieldlist_walk(Path, TplName, FieldList, TupleCb) ->
	tuple_fieldlist_walk(Path, TplName, FieldList, TupleCb, []).

tuple_fieldlist_walk(_Path, _TplName, [], _TplCb, OutList) ->
	OutList;
tuple_fieldlist_walk(Path, TplName, [Head|List], TupleCb, OutList) ->
	if
		is_tuple(Head) ->
			NewHead = tuple_walk(Path++[TplName], Head, TupleCb);
		is_list(Head) ->
			NewHead = tuple_walk(Path++[TplName], Head, TupleCb);
		true ->
			NewHead = Head
	end,
	tuple_fieldlist_walk(Path, TplName, List, TupleCb, OutList++[NewHead]).


tuple_walk_print_cb(Path, Tpl) when is_list(Path), is_tuple(Tpl) ->
	io:format("~p:~p~n", [Path, Tpl]),
	Tpl.
