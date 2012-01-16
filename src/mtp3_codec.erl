% ITU-T Q.704 (MTP Level 3) coding / decoding

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

-module(mtp3_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("mtp3.hrl").

-export([parse_mtp3_msg/1, encode_mtp3_msg/1]).

-compile({parse_transform, exprecs}).
-export_records([mtp3_routing_label, mtp3_msg]).

% Parse standard routing label according to Section 2.2 of ITU-T Q.704
parse_mtp3_routing_label(_, LabelBin) when is_binary(LabelBin) ->
	<<Sls:4/big, Opc:14/big, Dpc:14/big, Remain/binary>> = LabelBin,
	{ok, #mtp3_routing_label{sig_link_sel = Sls, origin_pc = Opc, dest_pc = Dpc}, Remain}.

parse_mtp3_msg(DataBin) when is_binary(DataBin) ->
	<<NetInd:2, 0:2, ServiceInd:4, Remain/binary>> = DataBin,
	{ok, RoutLbl, Payload} = parse_mtp3_routing_label(ServiceInd, Remain),
	PayloadDec = decode_payload(ServiceInd, Payload),
	#mtp3_msg{network_ind = NetInd, service_ind = ServiceInd, routing_label = RoutLbl,
		  payload = PayloadDec}.


encode_mtp3_routing_label(#mtp3_routing_label{sig_link_sel = Sls, origin_pc = OpcIn,
					      dest_pc = DpcIn}) ->
	Opc = osmo_util:pointcode2int(OpcIn),
	Dpc = osmo_util:pointcode2int(DpcIn),
	<<Sls:4/big, Opc:14/big, Dpc:14/big>>.

encode_mtp3_msg(#mtp3_msg{network_ind = NetInd, service_ind = ServiceInd,
			  routing_label = RoutLbl, payload = Payload}) ->
	RoutLblBin = encode_mtp3_routing_label(RoutLbl),
	PayloadBin = payload_to_binary(ServiceInd, Payload),
	<<NetInd:2, 0:2, ServiceInd:4, RoutLblBin/binary, PayloadBin/binary>>.


decode_payload(?MTP3_SERV_MTN, Payload) ->
	<<H0:4, H1:4, _:4, Len:4, TP/binary>> = Payload,
	#mtp3mg_msg{h0 = H0, h1 = H1, payload = TP};
decode_payload(?MTP3_SERV_MGMT, Payload) ->
	<<H0:4, H1:4, Remain/binary>> = Payload,
	#mtp3mg_msg{h0 = H0, h1 = H1, payload = Payload};
decode_payload(_, Payload) ->
	Payload.

payload_to_binary(?MTP3_SERV_MTN, #mtp3mg_msg{h0=H0, h1=H1, payload=TP}) ->
	Len = byte_size(TP),
	<<H0:4, H1:4, 0:4, Len:4, TP/binary>>;
payload_to_binary(?MTP3_SERV_MGMT, #mtp3mg_msg{h0=H0, h1=H1, payload=Payload}) ->
	<<H0:4, H1:4, Payload/binary>>;
payload_to_binary(_, Whatever) ->
	Whatever.

