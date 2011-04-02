% M3UA in accordance with RFC4666 (http://tools.ietf.org/html/rfc4666)

-define(M3UA_MSGC_MGMT,			0).	% Management
-define(M3UA_MSGC_TRANSFER,		1).	% Transfer
-define(M3UA_MSGC_SSNM,			2).	% SS7 Signalling Network Management
-define(M3UA_MSGC_ASPSM,		3).	% ASP State Management
-define(M3UA_MSGC_ASPTM,		4).	% ASP Traffic Maintenance
-define(M3UA_MSGC_RKM,			9).	% Routing Key Management

-define(M3UA_MSGT_MGMT_ERR,		0).
-define(M3UA_MSGT_MGMT_NTFY,		1).

-define(M3UA_MSGT_XFR_DATA,		1).

-define(M3UA_MSGT_SSNM_DUNA,		1).	% Destination Unavailable
-define(M3UA_MSGT_SSNM_DAVA,		2).	% Destination Available
-define(M3UA_MSGT_SSNM_DAUD,		3).	% Destination State Audit
-define(M3UA_MSGT_SSNM_SCON,		4).	% Signalling Congestion
-define(M3UA_MSGT_SSNM_DUPU,		5).	% Destination User Part Unavailable
-define(M3UA_MSGT_SSNM_DRST,		6).	% Destination Restricted

-define(M3UA_MSGT_ASPSM_ASPUP,		1).	% ASP Up
-define(M3UA_MSGT_ASPSM_ASPDN,		2).	% ASP Down
-define(M3UA_MSGT_ASPSM_BEAT,		3).	% Heartbeat
-define(M3UA_MSGT_ASPSM_ASPUP_ACK,	4).	% ASP Up Acknowledgement
-define(M3UA_MSGT_ASPSM_ASPDN_ACK,	5).	% ASP Down Acknowledgement
-define(M3UA_MSGT_ASPSM_BEAT_ACK,	6).	% Heartbeat Acknowledgement

-define(M3UA_MSGT_ASPTM_ASPAC,		1).	% ASP Active
-define(M3UA_MSGT_ASPTM_ASPIA,		2).	% ASP Inactive
-define(M3UA_MSGT_ASPTM_ASPAC_ACK,	3).	% ASP Active Acknowledgement
-define(M3UA_MSGT_ASPTM_ASPIA_ACK,	3).	% ASP Inactive Acknowledgement

-define(M3UA_MSGT_RKM_REG_REQ,		1).	% Registration Request
-define(M3UA_MSGT_RKM_REG_RSP,		2).	% Registration Response
-define(M3UA_MSGT_RKM_DEREG_REQ,	3).	% Deregistration Request
-define(M3UA_MSGT_RKM_DEREG_RSP,	4).	% Deregistration Response

-define(M3UA_IEI_INFO_STRING,		16#0004).
-define(M3UA_IEI_ROUTE_CTX,		16#0006).
-define(M3UA_IEI_DIAG_INFO,		16#0007).
-define(M3UA_IEI_HEARTB_DATA,		16#0009).
-define(M3UA_IEI_TRAF_MODE_TYPE,	16#000b).
-define(M3UA_IEI_ERR_CODE,		16#000c).
-define(M3UA_IEI_STATUS,		16#000d).
-define(M3UA_IEI_ASP_ID,		16#0011).
-define(M3UA_IEI_AFFECTED_PC,		16#0012).
-define(M3UA_IEI_CORR_ID,		16#0013).
% M3UA-Specific parameters
-define(M3UA_IEI_NET_APPEARANCE,	16#0200).
-define(M3UA_IEI_USER_CAUSE,		16#0204).
-define(M3UA_IEI_CONGESTION_IND,	16#0205).
-define(M3UA_IEI_CONCERNED_IND,		16#0206).
-define(M3UA_IEI_ROUTING_KEY,		16#0207).
-define(M3UA_IEI_REG_RESULT,		16#0208).
-define(M3UA_IEI_DEREG_RESULT,		16#0209).
-define(M3UA_IEI_LOCAL_RKEY_ID,		16#020a).
-define(M3UA_IEI_DEST_PC,		16#020b).
-define(M3UA_IEI_SERVICE_IND,		16#020c).
-define(M3UA_IEI_ORIG_PC_LIST,		16#020e).
-define(M3UA_IEI_PROTOCOL_DATA,		16#0210).
-define(M3UA_IEI_REG_STATUS,		16#0212).
-define(M3UA_IEI_DEREG_STATUS,		16#0213).

-record(m3ua_msg, {
	 version,
	 msg_class,
	 msg_type,
	 msg_length,
	 payload
	}).

