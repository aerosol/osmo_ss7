% SCCP connectionles control (SCLC)

% (C) 2010 by Harald Welte <laforge@gnumonks.org>
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

-module(sccp_sclc).
-behaviour(gen_fsm).
-export([start_link/1]).


% TODO: 

start_link(Init) ->
	gen_fsm:start_link({local, sccp_sclc}, sccp_sclc, Init, []).

init(InitData) ->
	{ok, idle, {[], InitData}}.

idle(connectionless_msg, 
%idle(changes_needed, LoopDat) ->
idle({'N', 'UNITDATA', request, Parms}, LoopDat) ->
	% assign SLS
	gen_fsm:send_event(sccp_scrc, connectionless_msg
	{next_state, idle, LoopDat};
%idle(scmg_msg, LoopDat) ->
idle(routing_failure, LoopDat) ->
	
