% Osmocom adaptor to interface the IPA core with osmo_ss7

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

-module(ss7_link_ipa_client).
-author('Harald Welte <laforge@gnumonks.org>').
-behavior(gen_server).

-include_lib("osmo_ss7/include/osmo_util.hrl").
%-include_lib("osmo_ss7/include/ipa.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/osmo_ss7.hrl").
-include_lib("osmo_ss7/include/mtp3.hrl").

-export([start_link/1, init/1]).

-export([handle_cast/2, handle_info/2]).

-record(loop_dat, {
	 ipa_pid,
	 socket,
	 link
	}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(L = #sigtran_link{type = ipa_client, name = Name, linkset_name = LinksetName,
			sls = Sls}) ->
	% start the IPA link to the SG
	ok = ss7_links:register_link(LinksetName, Sls, Name),
	{ok, LoopDat2} = reconnect(#loop_dat{link = L}),
	{ok, LoopDat2}.

handle_info({ipa_closed, {_Sock, _Stream}}, LoopDat) ->
	set_link_state(LoopDat, down),
	{ok, LoopDat2} = reconnect(LoopDat),
	{no_reply, LoopDat2}.

handle_cast(#primitive{subsystem='MTP', gen_name='TRANSFER', spec_name=request,
		       parameters = #mtp3_msg{service_ind = ?MTP3_SERV_SCCP,
		       			    payload = Data}}, LoopDat) ->
	#loop_dat{socket = Socket, ipa_pid = Pid} = LoopDat,
	Pid ! {ipa_send, Socket, 253, Data},
	{no_reply, LoopDat}.

reconnect(LoopDat = #loop_dat{link=Link}) ->
	#sigtran_link{local = Local, remote = Remote} = Link,
	#sigtran_peer{ip = LocalIp, port = LocalPort} = Local,
	#sigtran_peer{ip = RemoteIp, port = RemotePort} = Remote,
	case ipa_proto:connect(RemoteIp, RemotePort, [], 10000) of
		{ok, {Socket, IpaPid}} ->
			set_link_state(LoopDat, up),
			ipa_proto:register_stream(Socket, 253, {callback_fn, fun ipa_tx_to_sccp/4, []}),
			ipa_proto:unblock(Socket),
			{ok, LoopDat#loop_dat{ipa_pid=IpaPid, socket=Socket}};
		{error, Reason} ->
			io:format("Reconnecting TCP (~w)~n", [Reason]),
			reconnect(LoopDat)
	end.

set_link_state(#loop_dat{link = #sigtran_link{linkset_name = LinksetName, sls = Sls}}, State) ->
	ss7_links:set_link_state(LinksetName, Sls, State).

% Callback that we pass to the ipa_proto, which it will call when it wants to
% send a primitive up the stack to SCCP
ipa_tx_to_sccp(_Socket, 253, Data, _Args) ->
	osmo_ss7:mtp3_rx(#mtp3_msg{service_ind=?MTP3_SERV_SCCP, payload=Data}).
