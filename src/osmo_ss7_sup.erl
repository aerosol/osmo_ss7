% OTP Supervisor for Osmocom SCCP

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

-module(osmo_ss7_sup).
-behavior(supervisor).

-export([start_link/0, add_mtp_link/1]).
-export([init/1]).

-include_lib("osmo_ss7/include/osmo_ss7.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [{debug, [trace]}]).

init(Args) ->
	LinksChild = {ss7_links, {ss7_links, start_link, []},
		     permanent, 2000, worker, [ss7_links]},
	{ok,{{one_for_one,60,600}, [LinksChild]}}.

% Add a m3ua link to this supervisor
add_mtp_link(L=#sigtran_link{type = m3ua, name = Name,
			   local = Local, remote = Remote}) ->
	ChildName = list_to_atom("ss7_link_m3ua_" ++ Name),
	ChildSpec = {ChildName, {ss7_link_m3ua, start_link, [L]},
		     permanent, infinity, worker, [ss7_link_m3ua]},
	supervisor:start_child(?MODULE, ChildSpec);
add_mtp_link([]) ->
	ok;
add_mtp_link([Head|Tail]) ->
	add_mtp_link(Head, Tail).
add_mtp_link(Head, Tail) ->
	{ok, _Child} = add_mtp_link(Head),
	add_mtp_link(Tail).
