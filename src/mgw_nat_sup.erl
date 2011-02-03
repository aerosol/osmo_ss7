% OTP Supervisor for MGW NAT

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
% (C) 2011 OnWaves
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

-module(mgw_nat_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
	{ok, MscLocalIp} = application:get_env(msc_local_ip),
	{ok, MscLocalPort} = application:get_env(msc_local_port),
	{ok, MscRemoteIp} = application:get_env(msc_remote_ip),
	{ok, StpRemoteIp} = application:get_env(stp_remote_ip),
	{ok, StpRemotePort} = application:get_env(stp_remote_port),
	SctpHdlrArgs =	[MscLocalIp, MscLocalPort, MscRemoteIp,
			 StpRemoteIp, StpRemotePort],
	MgwChild = {mgw_nat_usr, {mgw_nat_usr, start_link, [SctpHdlrArgs]},
		    permanent, 2000, worker, [mgw_nat_usr, sctp_handler, mgw_nat]},
	{ok,{{one_for_all,1,1}, [MgwChild]}}.
