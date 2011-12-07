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
-export([tuple_walk/3, tuple_walk_print_cb/3]).
-export([make_prim/4, make_prim/3]).
-export([pointcode2int/1, pointcode2int/2, pointcode_fmt/2]).

-include("osmo_util.hrl").

-compile({parse_transform, exprecs}).
-export_records([primitive]).

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
tuple_walk(Tpl, TupleCb, Args) when is_tuple(Tpl), is_function(TupleCb),
				    is_list(Args) ->
	tuple_walk([], Tpl, TupleCb, Args).

tuple_walk(Path, Tpl, TupleCb, Args) when is_list(Path), is_tuple(Tpl),
					  is_list(Args) ->
	% call Callback
	NewTpl = TupleCb(Path, Tpl, Args),
	[TplName|TplList] = tuple_to_list(NewTpl),
	NewTplList = tuple_fieldlist_walk(Path, TplName, TplList, TupleCb, Args),
	list_to_tuple([TplName|NewTplList]);
tuple_walk(Path, TplL, TupleCb, Args) when is_list(Path), is_list(TplL),
					   is_list(Args) ->
	tuple_walk_list(Path, TplL, TupleCb, Args, []).

tuple_walk_list(_Path, [], _TupleCb, _Args, OutList) ->
	OutList;
tuple_walk_list(Path, [Head|Tail], TupleCb, Args, OutList) ->
	if
		is_tuple(Head) ->
			NewHead = tuple_walk(Path, Head, TupleCb, Args);
		is_list(Head) ->
			NewHead = tuple_walk(Path, Head, TupleCb, Args);
		true ->
			NewHead = Head
	end,
	tuple_walk_list(Path, Tail, TupleCb, Args, OutList++[NewHead]).


tuple_fieldlist_walk(Path, TplName, FieldList, TupleCb, Args) ->
	tuple_fieldlist_walk(Path, TplName, FieldList, TupleCb, Args, []).

tuple_fieldlist_walk(_Path, _TplName, [], _TplCb, _Args, OutList) ->
	OutList;
tuple_fieldlist_walk(Path, TplName, [Head|List], TupleCb, Args, OutList) ->
	if
		is_tuple(Head) ->
			NewHead = tuple_walk(Path++[TplName], Head, TupleCb, Args);
		is_list(Head) ->
			NewHead = tuple_walk(Path++[TplName], Head, TupleCb, Args);
		true ->
			NewHead = Head
	end,
	tuple_fieldlist_walk(Path, TplName, List, TupleCb, Args, OutList++[NewHead]).


tuple_walk_print_cb(Path, Tpl, _Args) when is_list(Path), is_tuple(Tpl) ->
	io:format("~p:~p~n", [Path, Tpl]),
	Tpl.

% helper function to create a #primitive record
make_prim(Subsys, GenName, SpecName) ->
	make_prim(Subsys, GenName, SpecName, []).
make_prim(Subsys, GenName, SpecName, Param) ->
	#primitive{subsystem = Subsys, gen_name = GenName,
		   spec_name = SpecName, parameters = Param}.

% parse a 3-tuple pointcode into a raw integer
pointcode2int(#pointcode{repr=Type, value=Value}) ->
	pointcode2int(Type, Value);
pointcode2int({Std, Param}) ->
	pointcode2int(Std, Param).

pointcode2int(itu, {A, B, C}) ->
	<<PcInt:14/big>> = <<A:3, B:8, C:3>>,
	PcInt;
pointcode2int(ansi, {A, B, C}) ->
	<<PcInt:24/big>> = <<A:8, B:8, C:8>>,
	PcInt;
pointcode2int(ttc, {A, B, C}) ->
	<<PcInt:16/big>> = <<A:5, B:4, C:7>>,
	PcInt.

% format a point-code into a 3-tuple according to the standard used
pointcode_fmt(Std, P) when is_binary(P) ->
	<<PcInt/integer>> = P,
	pointcode_fmt(Std, PcInt);
pointcode_fmt(itu, PcInt) when is_integer(PcInt) ->
	<<A:3, B:8, C:3>> = <<PcInt:14/big>>,
	{pointcode, itu, {A, B, C}};
pointcode_fmt(ansi, PcInt) ->
	<<A:8, B:8, C:8>> = <<PcInt:24/big>>,
	{pointcode, ansi, {A, B, C}};
pointcode_fmt(ttc, PcInt) ->
	<<A:5, B:4, C:7>> = <<PcInt:16/big>>,
	{pointcode, ttc, {A, B, C}}.
