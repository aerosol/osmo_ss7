% SS7 service handler for dumping MTP3 payload (testing)

% (C) 2010-2011 by Harald Welte <laforge@gnumonks.org>
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
% You should have received a copy of the GNU Affero General Public
% License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(ss7_service_dump).
-behaviour(gen_fsm).
-export([start_link/1, init/1, idle/2]).

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/mtp3.hrl").
-include_lib("osmo_ss7/include/isup.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").

-record(loop_dat, {
	ssns
}).

start_link(Ssns) ->
	gen_fsm:start_link(ss7_service_dump, [Ssns], []).

init(Ssn) when is_integer(Ssn) ->
	init([Ssn]);
init(Ssns) when is_list(Ssns) ->
	bind_services(Ssns),
	{ok, idle, #loop_dat{ssns = Ssns}}.

bind_services([]) ->
	ok;
bind_services([Head|Tail]) ->
	ok = ss7_links:bind_service(Head, "ss7_service_dump"),
	bind_services(Tail).

parse_mtp3(#mtp3_msg{service_ind = ?MTP3_SERV_SCCP, payload = Payload}) ->
	sccp_codec:parse_sccp_msg(Payload);
parse_mtp3(#mtp3_msg{service_ind = ?MTP3_SERV_ISUP, payload = Payload}) ->
	isup_codec:parse_isup_msg(Payload).

idle(#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		spec_name = indication, parameters = Mtp3Msg}, LoopDat) ->
	io:format("ss7_service_dump in: ~p~n", [Mtp3Msg]),
	try parse_mtp3(Mtp3Msg) of
		Parsed ->
			io:format("ss7_service_dump out: ~p~n", [Parsed])
	catch
		_ ->
			io:format("ss7_service_dump parser error~n")
	end,
	{next_state, idle, LoopDat}.
