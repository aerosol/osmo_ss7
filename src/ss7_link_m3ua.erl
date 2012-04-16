% Osmocom adaptor to interface the M3UA core with osmo_sccp

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

-module(ss7_link_m3ua).
-author('Harald Welte <laforge@gnumonks.org>').
-behavior(gen_server).

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/m3ua.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/osmo_ss7.hrl").

-export([start_link/1, init/1]).

-export([handle_cast/2]).

-record(loop_dat, {
	 m3ua_pid,
	 link
	}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, [{debug, [trace]}]).

init(L = #sigtran_link{type = m3ua, name = Name, linkset_name = LinksetName,
		       sls = Sls, local = Local, remote = Remote}) ->
	#sigtran_peer{ip = LocalIp, port = LocalPort} = Local,
	#sigtran_peer{ip = RemoteIp, port = RemotePort} = Remote,
	% start the M3UA link to the SG
	Opts = [{user_pid, self()}, {sctp_remote_ip, RemoteIp},
		{sctp_remote_port, RemotePort}, {sctp_local_port, LocalPort},
		{user_fun, fun m3ua_tx_to_user/2}, {user_args, self()}],
	{ok, M3uaPid} = m3ua_core:start_link(Opts),
	% FIXME: register this link with SCCP_SCRC
	ok = ss7_links:register_link(LinksetName, Sls, Name),
	{ok, #loop_dat{m3ua_pid = M3uaPid, link = L}}.

%	% instantiate SCCP routing instance
%	{ok, ScrcPid} = sccp_scrc:start_link([{mtp_tx_action, {callback_fn, fun scrc_tx_to_mtp/2, M3uaPid}}]),
%	loop(#loop_dat{m3ua_pid = M3uaPid, scrc_pid = ScrcPid}).


set_link_state(#sigtran_link{linkset_name = LinksetName, sls = Sls}, State) ->
	ok = ss7_links:set_link_state(LinksetName, Sls, State).

scrc_tx_to_mtp(Prim, Args) ->
	M3uaPid = Args,
	gen_fsm:send_event(M3uaPid, Prim).

% Callback that we pass to the m3ua_core, which it will call when it wants to
% send a primitive up the stack to SCCP
m3ua_tx_to_user(P=#primitive{subsystem = 'MTP'}, Args) ->
	% send it directly to the 'service' that has bound
	ss7_links:mtp3_rx(P);
m3ua_tx_to_user(P=#primitive{subsystem = 'M'}, Args) ->
	% send management primitives into the m3ua_link process
	UserPid = Args,
	gen_server:cast(UserPid, P).

handle_cast(P = #primitive{subsystem = 'MTP', gen_name = 'TRANSFER', spec_name = request}, L) ->
	scrc_tx_to_mtp(P, L#loop_dat.m3ua_pid),
	{noreply, L};
% This is what we receive from m3ua_tx_to_user/2
handle_cast(#primitive{subsystem = 'M', gen_name = 'SCTP_ESTABLISH', spec_name = confirm}, L) ->
	io:format("~p: SCTP_ESTABLISH.ind -> ASP_UP.req~n", [?MODULE]),
	gen_fsm:send_event(L#loop_dat.m3ua_pid, osmo_util:make_prim('M','ASP_UP',request)),
	{noreply, L};
handle_cast(#primitive{subsystem = 'M', gen_name = 'ASP_UP', spec_name = confirm}, L) ->
	io:format("~p: ASP_UP.ind -> ASP_ACTIVE.req~n", [?MODULE]),
	set_link_state(L#loop_dat.link, up),
	gen_fsm:send_event(L#loop_dat.m3ua_pid, osmo_util:make_prim('M','ASP_ACTIVE',request)),
	{noreply, L};
handle_cast(#primitive{subsystem = 'M', gen_name = 'ASP_ACTIVE', spec_name = confirm}, L) ->
	io:format("~p: ASP_ACTIVE.ind - M3UA now active and ready~n", [?MODULE]),
	set_link_state(L#loop_dat.link, active),
	%tx_sccp_udt(L#loop_dat.scrc_pid),
	{noreply, L};
handle_cast(#primitive{subsystem = 'M', gen_name = 'ASP_DOWN'}, L) ->
	io:format("~p: ASP_DOWN.ind~n", [?MODULE]),
	set_link_state(L#loop_dat.link, down),
	{noreply, L};
handle_cast(#primitive{subsystem = 'M', gen_name = 'ASP_INACTIVE'}, L) ->
	io:format("~p: ASP_INACTIVE.ind~n", [?MODULE]),
	set_link_state(L#loop_dat.link, up),
	{noreply, L};
handle_cast(P, L) ->
	io:format("~p: Ignoring M3UA prim ~p~n", [?MODULE, P]),
	{noreply, L}.

terminate(Reason, _S) ->
	io:format("terminating ~p with reason ~p", [?MODULE, Reason]),
	ok.

tx_sccp_udt(ScrcPid) ->
	CallingP = #sccp_addr{ssn = ?SCCP_SSN_MSC, point_code = osmo_util:pointcode2int(itu, {1,2,2})},
	CalledP = #sccp_addr{ssn = ?SCCP_SSN_HLR, point_code = osmo_util:pointcode2int(itu, {1,1,1})},
	Data = <<1,2,3,4>>,
	Opts = [{protocol_class, 0}, {called_party_addr, CalledP},
		{calling_party_addr, CallingP}, {user_data, Data}],
	io:format("~p: Sending N-UNITDATA.req to SCRC~n", [?MODULE]),
	gen_fsm:send_event(ScrcPid, osmo_util:make_prim('N','UNITDATA',request,Opts)).

