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

-define(MSC_LOCAL_IP,		any).
-define(MSC_LOCAL_PORT,		2904).
-define(MSC_REMOTE_IP,		{172,16,1,81}).
-define(STP_REMOTE_IP,		{172,16,249,20}).
-define(STP_REMOTE_PORT,	2904).

-define(SCTP_HDLR_ARGS,	[?MSC_LOCAL_IP, ?MSC_LOCAL_PORT, ?MSC_REMOTE_IP,
			 ?STP_REMOTE_IP, ?STP_REMOTE_PORT]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
	MgwChild = {mgw_nat_usr, {mgw_nat_usr, start_link, ?SCTP_HDLR_ARGS},
		    permanent, 2000, worker, [mgw_nat_usr, sctp_handler, mgw_nat]},
	{ok,{{one_for_all,1,1}, [MgwChild]}}.
