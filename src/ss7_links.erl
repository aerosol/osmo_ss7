% Internal SCCP link database keeping

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
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(ss7_links).
-behaviour(gen_server).

-include_lib("osmo_ss7/include/mtp3.hrl").
-include_lib("osmo_ss7/include/osmo_util.hrl").

% gen_fsm callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

% our published API
-export([start_link/0]).

% client functions, may internally talk to our sccp_user server
-export([register_linkset/3, unregister_linkset/1]).
-export([register_link/3, unregister_link/2, set_link_state/3]).
-export([bind_service/2, unbind_service/1]).

-export([get_pid_for_link/2, get_pid_for_dpc_sls/2,
	 mtp3_tx/1, mtp3_tx/2,
	 get_linkset_for_dpc/1, get_opc_for_linkset/1, is_pc_local/1,
	 get_user_pid_for_service/1, mtp3_rx/1, dump/0]).

-type link_state() :: down | up_inactive | active.
-opaque tid()      :: integer().
-type error()	   :: { error, term() }.
-type osmo_point_code() :: non_neg_integer().

-record(slink, {
	key,		% {linkset_name, sls}
	name,		% name of the link
	linkset_name,	% name of the linkset to which we belong
	sls,
	user_pid :: pid(), % Pid handling MTP-TRANSFER primitives
	state :: link_state()
}).

-record(slinkset, {
	name :: string(),		% name of the linkset
	local_pc :: non_neg_integer(),	% local point code
	remote_pc :: non_neg_integer(),	% remote point code
	user_pid :: pid(),
	state :: link_state(),
	active_sls :: list()	% list of Sls of currently active links
}).

-record(service, {
	name :: string(),
	service_nr :: non_neg_integer(),
	user_pid :: pid()
}).

-record(su_state, {
	linkset_tbl :: tid(),
	link_tbl :: tid(),
	service_tbl :: tid()
}).


% initialization code

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{debug, [trace]}]).

init(_Arg) ->
	LinksetTbl = ets:new(ss7_linksets, [ordered_set, named_table,
					     {keypos, #slinkset.name}]),
	ServiceTbl = ets:new(mtp3_services, [ordered_set, named_table,
				{keypos, #service.service_nr}]),

	% create a named table so we can query without reference directly
	% within client/caller process
	LinkTbl = ets:new(ss7_link_table, [ordered_set, named_table,
					    {keypos, #slink.key}]),
	process_flag(trap_exit, true),
	{ok, #su_state{linkset_tbl = LinksetTbl, link_tbl = LinkTbl,
			service_tbl = ServiceTbl}}.

% client side API

% all write operations go through gen_server:call(), as only the ?MODULE
% process has permission to modify the table content

-spec register_linkset(non_neg_integer(), non_neg_integer(), string())
						-> ok | error().

register_linkset(LocalPc, RemotePc, Name) ->
	gen_server:call(?MODULE, {register_linkset, {LocalPc, RemotePc, Name}}).

-spec unregister_linkset(string()) -> ok | error().

unregister_linkset(Name) ->
	gen_server:call(?MODULE, {unregister_linkset, {Name}}).

-spec register_link(string(), non_neg_integer(), string()) ->
					ok | error().

register_link(LinksetName, Sls, Name) ->
	gen_server:call(?MODULE, {register_link, {LinksetName, Sls, Name}}).

-spec unregister_link(string(), non_neg_integer()) ->
					ok | error().

unregister_link(LinksetName, Sls) ->
	gen_server:call(?MODULE, {unregister_link, {LinksetName, Sls}}).

-spec set_link_state(string(), non_neg_integer(), link_state()) ->
					ok | error().

set_link_state(LinksetName, Sls, State) ->
	gen_server:call(?MODULE, {set_link_state, {LinksetName, Sls, State}}).

-spec bind_service(non_neg_integer(), string()) ->
					ok | error().

% bind a service (such as ISUP, SCCP) to the MTP3 link manager
bind_service(ServiceNum, ServiceName) ->
	gen_server:call(?MODULE, {bind_service, {ServiceNum, ServiceName}}).

-spec unbind_service(non_neg_integer()) ->
					ok | error().

% unbind a service (such as ISUP, SCCP) from the MTP3 link manager
unbind_service(ServiceNum) ->
	gen_server:call(?MODULE, {unbind_service, {ServiceNum}}).

% the lookup functions can directly use the ets named_table from within
% the client process, no need to go through a synchronous IPC

-spec get_user_pid_for_service(non_neg_integer()) ->
	{ok, pid()} | error().

get_user_pid_for_service(Service) when is_integer(Service) ->
	case ets:lookup(mtp3_services, Service) of
	    [#service{user_pid=Pid}] ->
		{ok, Pid};
	    [] ->
		{error, no_such_service}
	end.

-spec get_pid_for_link(string(), non_neg_integer()) ->
	{ok, pid()} | error().

get_pid_for_link(LinksetName, Sls) when is_list(LinksetName), is_integer(Sls) ->
	case ets:lookup(ss7_link_table, {LinksetName, Sls}) of
	    [#slink{user_pid = Pid}] ->	
		% FIXME: check the link state 
		{ok, Pid};
	    _ ->
		{error, no_such_link}
	end.

-spec get_linkset_for_dpc(osmo_point_code()) ->
	{ok, string()} | error().

% Resolve linkset name directly connected to given point code
get_linkset_for_dpc(DpcIn) ->
	Dpc = osmo_util:pointcode2int(DpcIn),
	Ret = ets:match_object(ss7_linksets,
			       #slinkset{remote_pc = Dpc, _ = '_'}),
	case Ret of
	    [] ->
		{error, undefined};
	    [#slinkset{name=Name}|_Tail] ->
		{ok, Name}
	end.

-spec get_pid_for_dpc_sls(osmo_point_code(), non_neg_integer()) ->
	{ok, string()} | error().

% resolve link-handler Pid for given (directly connected) point code/sls
get_pid_for_dpc_sls(DpcIn, Sls) when is_integer(Sls) ->
	Dpc = osmo_util:pointcode2int(DpcIn),
	case get_linkset_for_dpc(Dpc) of
	    {error, Err} ->
		{error, Err};
	    {ok, LinksetName} ->
		get_pid_for_link(LinksetName, Sls)
	end.

-spec get_opc_for_linkset(string()) -> non_neg_integer() | undefined.

% the the local point code for a given linkset
get_opc_for_linkset(LsName) when is_list(LsName) ->
	case ets:lookup(ss7_linksets, LsName) of
	    [#slinkset{local_pc = Opc}|_Tail] ->
		Opc;
	    _ ->
		undefined
	end.

% determine if a given point code is local
is_pc_local(Pc) when is_integer(Pc) ->
	Ret = ets:match_object(ss7_linksets,
			       #slinkset{local_pc = Pc, _ = '_'}),
	case Ret of
	    [#slinkset{}] ->
		true;
	    _ ->
		false
	end.

% process a received message on an underlying link
mtp3_rx(Mtp3) when is_record(Mtp3, mtp3_msg) ->
	mtp3_rx(osmo_util:make_prim('MTP', 'TRANSFER', indication, Mtp3));
% FIXME: PAUSE/RESUME/STATUS handling
mtp3_rx(#primitive{subsystem='MTP', gen_name='PAUSE', spec_name=indication}) ->
	ok;
mtp3_rx(#primitive{subsystem='MTP', gen_name='RESUME', spec_name=indication}) ->
	ok;
mtp3_rx(#primitive{subsystem='MTP', gen_name='STATUS', spec_name=indication}) ->
	ok;
mtp3_rx(P = #primitive{parameters=#mtp3_msg{service_ind=Serv}}) ->
	case ets:lookup(mtp3_services, Serv) of
	     [#service{user_pid = Pid}] ->
		gen_server:cast(Pid, P);
	    _ ->
		% FIXME: send back some error message on MTP level
		ok
	end.


% transmit a MTP3 message via any of the avaliable links for the DPC
mtp3_tx(Mtp3 = #mtp3_msg{routing_label = RoutLbl}, Link) ->
	#mtp3_routing_label{sig_link_sel = Sls} = RoutLbl,
	% discover the link through which we shall send
	case get_pid_for_link(Link, Sls) of
	    {error, Error} ->
		{error, Error};
	    {ok, Pid} ->
		    gen_server:cast(Pid,
				osmo_util:make_prim('MTP', 'TRANSFER',
						    request, Mtp3))
	end.


% transmit a MTP3 message via any of the avaliable links for the DPC
mtp3_tx(Mtp3 = #mtp3_msg{routing_label = RoutLbl}) ->
	#mtp3_routing_label{dest_pc = Dpc, sig_link_sel = Sls} = RoutLbl,
	% discover the link through which we shall send
	case ss7_routes:route_dpc(Dpc) of
	    {error, Error} ->
		{error, Error};
	    {ok, Linkset} ->
		{ok, Pid} = get_pid_for_link(Linkset, Sls),
		gen_server:cast(Pid,
				osmo_util:make_prim('MTP', 'TRANSFER',
						    request, Mtp3))
	end.

dump() ->
	List = ets:tab2list(ss7_linksets),
	dump_linksets(List),
	SList = ets:tab2list(mtp3_services),
	dump_services(SList).

dump_linksets([]) ->
	ok;
dump_linksets([Head|Tail]) when is_record(Head, slinkset) ->
	dump_single_linkset(Head),
	dump_linksets(Tail).

dump_single_linkset(Sls) when is_record(Sls, slinkset) ->
	#slinkset{name = Name, local_pc = Lpc, remote_pc = Rpc,
		  user_pid = Pid, state = State} = Sls,
	io:format("Linkset ~p, Local PC: ~p, Remote PC: ~p, Pid: ~p, State: ~p~n",
		  [Name, Lpc, Rpc, Pid, State]),
	dump_linkset_links(Name).

dump_linkset_links(Name) ->
	List = ets:match_object(ss7_link_table,
				#slink{key={Name,'_'}, _='_'}),
	dump_links(List).

dump_links([]) ->
	ok;
dump_links([Head|Tail]) when is_record(Head, slink) ->
	#slink{name = Name, sls = Sls, state = State, user_pid = Pid} = Head,
	io:format("  Link ~p, SLS: ~p, Pid: ~p, State: ~p~n",
		  [Name, Sls, Pid, State]),
	dump_links(Tail).

dump_services([]) ->
	ok;
dump_services([Head|Tail]) when is_record(Head, service) ->
	#service{name = Name, user_pid = Pid, service_nr = Nr} = Head,
	io:format("Service ~p bound to ~p (Pid ~p)~n", [Nr, Name, Pid]),
	dump_services(Tail).

% server side code

handle_call({register_linkset, {LocalPc, RemotePc, Name}},
				{FromPid, _FromRef}, S) ->
	#su_state{linkset_tbl = Tbl} = S,
	Ls = #slinkset{local_pc = LocalPc, remote_pc = RemotePc,
		       name = Name, user_pid = FromPid,
		       state = down, active_sls=[]},
	case ets:insert_new(Tbl, Ls) of
	    false ->
		{reply, {error, ets_insert}, S};
	    _ ->
		% We need to trap the user Pid for EXIT
		% in order to automatically remove any links/linksets if
		% the user process dies
		%
		% we decided to keep Linksets as something like global
		% configuration around and not kill them in case the user who
		% created them has died.
		%link(FromPid),
		{reply, ok, S}
	end;

handle_call({unregister_linkset, {Name}}, {FromPid, _FromRef}, S) ->
	#su_state{linkset_tbl = Tbl} = S,
	ets:delete(Tbl, Name),
	{reply, ok, S};

handle_call({register_link, {LsName, Sls, Name}},
				{FromPid, _FromRef}, S) ->
	#su_state{linkset_tbl = LinksetTbl, link_tbl = LinkTbl} = S,
	% check if linkset actually exists
	case ets:lookup(LinksetTbl, LsName) of
	    [#slinkset{}] ->
		Link = #slink{name = Name, sls = Sls, state = down,
			      user_pid = FromPid, key = {LsName, Sls}},
		case ets:insert_new(LinkTbl, Link) of
		    false ->
			{reply, {error, link_exists}, S};
		    _ ->
			% We need to trap the user Pid for EXIT
			% in order to automatically remove any links if
			% the user process dies
			link(FromPid),
			{reply, ok, S}
		end;
	    _ ->
		{reply, {error, no_such_linkset}, S}
	end;

handle_call({unregister_link, {LsName, Sls}}, {FromPid, _FromRef}, S) ->
	#su_state{link_tbl = LinkTbl} = S,
	ets:delete(LinkTbl, {LsName, Sls}),
	{reply, ok, S};

handle_call({set_link_state, {LsName, Sls, State}}, {FromPid, _}, S) ->
	#su_state{link_tbl = LinkTbl} = S,
	case ets:lookup(LinkTbl, {LsName, Sls}) of
	    [] ->
		{reply, {error, no_such_link}, S};
	    [Link] ->
		NewLink = Link#slink{state = State},
		ets:insert(LinkTbl, NewLink),
		propagate_linkstate_to_linkset(LsName, Sls, State),
		{reply, ok, S}
	end;

handle_call({bind_service, {SNum, SName}}, {FromPid, _},
	    #su_state{service_tbl = ServTbl} = S) ->
	NewServ = #service{name = SName, service_nr = SNum,
			   user_pid = FromPid},
	case ets:insert_new(ServTbl, NewServ) of
	    false ->
		{reply, {error, ets_insert}, S};
	    _ ->
		% We need to trap the user Pid for EXIT
		% in order to automatically remove any links if
		% the user process dies
		link(FromPid),
		{reply, ok, S}
	end;
handle_call({unbind_service, {SNum}}, {FromPid, _},
	    #su_state{service_tbl = ServTbl} = S) ->
	ets:delete(ServTbl, SNum),
	{reply, ok, S}.

handle_cast(Info, S) ->
	error_logger:error_report(["unknown handle_cast",
				  {module, ?MODULE},
				  {info, Info}, {state, S}]),
	{noreply, S}.

handle_info({'EXIT', Pid, Reason}, S) ->
	io:format("EXIT from Process ~p (~p), cleaning up tables~n",
		  [Pid, Reason]),
	#su_state{linkset_tbl = LinksetTbl, link_tbl = LinkTbl,
		  service_tbl = ServiceTbl} = S,
	% we decided to keep Linksets as something like global
	% configuration around and not kill them in case the user who
	% created them has died.
	%ets:match_delete(LinksetTbl, #slinkset{user_pid = Pid, _='_'}),
	ets:match_delete(LinkTbl, #slink{user_pid = Pid, _='_'}),
	ets:match_delete(ServiceTbl, #service{user_pid = Pid, _='_'}),
	{noreply, S};
handle_info(Info, S) ->
	error_logger:error_report(["unknown handle_info",
				  {module, ?MODULE},
				  {info, Info}, {state, S}]),
	{noreply, S}.

terminate(Reason, _S) ->
	io:format("terminating ~p with reason ~p", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% update the active_sls state in a linkset after a link state chg
propagate_linkstate_to_linkset(LsName, Sls, State) ->
	case ets:lookup(ss7_linksets, LsName) of
	     [Ls = #slinkset{}] ->
	       #slinkset{active_sls = ActSls, remote_pc = Dpc} = Ls,
		case State of
		    active ->
			% add Sls to list (unique)
			ActSls2 = lists:usort([Sls|ActSls]);
		    _ ->
			% del Sls from list
			ActSls2 = lists:delete(Sls, ActSls)
		end,
		% compute the linkstate state
		case ActSls2 of
		    [] ->
			LsState = up_inactive,
			ss7_routes:delete_route(Dpc, 16#ffff, LsName);
		    _ ->
			LsState = active,
			ss7_routes:create_route(Dpc, 16#ffff, LsName)
		end,
		ets:insert(ss7_linksets,
			   Ls#slinkset{active_sls = ActSls2,
					state = LsState});
	    _ ->
		{error, ets_lookup}
	end.
