
-define(M2UA_PPID,	2).
-define(M2UA_PORT,	2904).

% RFC 3331 Section 3.1.3 Message Class
-define(M2UA_MSGC_MGMT,		0).	% Management Messages [IUA/M2UA/M3UA/SUA]
-define(M2UA_MSGC_TRANSFER,	1).	% Transfer Messages [M3UA]
-define(M2UA_MSGC_SSNM,		2).	% SS7 Signalling Network Management [M3UA/SUA]
-define(M2UA_MSGC_ASPSM,	3).	% ASP State Maintenance [IUA/M2UA/M3UA/SUA]
-define(M2UA_MSGC_ASPTM,	4).	% ASP Traffic Maintenance [IUA/M2UA/M3UA/SUA]
-define(M2UA_MSGC_QPTM,		5).	% Q.921/Q.931 Boundary Primitives Transport [IUA]
-define(M2UA_MSGC_MAUP,		6).	% MTP2 User Adaption [M2UA]
-define(M2UA_MSGC_CONNLESS,	7).	% Connectionless Messages [SUA]
-define(M2UA_MSGC_CONN,		8).	% Connection oriented messages [SUA]
-define(M2UA_MSGC_RKM,		9).	% Routing Key Management [M3UA]
-define(M2UA_MSGC_IIM,		10).	% Interface Identifier Management (M2UA)

% RFC 3331 Section 3.1.4 Message Type
-define(M2UA_MAUP_MSGT_RESERVED,	0).
-define(M2UA_MAUP_MSGT_DATA,		1).
-define(M2UA_MAUP_MSGT_EST_REQ,		2).
-define(M2UA_MAUP_MSGT_EST_CONF,	3).
-define(M2UA_MAUP_MSGT_REL_REQ,		4).
-define(M2UA_MAUP_MSGT_REL_CONF,	5).
-define(M2UA_MAUP_MSGT_REL_IND,		6).
-define(M2UA_MAUP_MSGT_STATE_REQ,	7).
-define(M2UA_MAUP_MSGT_STATE_CONF,	8).
-define(M2UA_MAUP_MSGT_STATE_IND,	9).
-define(M2UA_MAUP_MSGT_DATA_RETR_REQ,	10).
-define(M2UA_MAUP_MSGT_DATA_RETR_CONF,	11).
-define(M2UA_MAUP_MSGT_DATA_RETR_IND,	12).
-define(M2UA_MAUP_MSGT_DATA_RETR_COMPL_IND,	13).
-define(M2UA_MAUP_MSGT_CONG_IND,	14).
-define(M2UA_MAUP_MSGT_DATA_ACK,	15).

-define(M2UA_ASPSM_MSGT_UP,		0).
-define(M2UA_ASPSM_MSGT_DOWN,		1).
-define(M2UA_ASPSM_MSGT_BEAT,		2).
-define(M2UA_ASPSM_MSGT_UP_ACK,		3).
-define(M2UA_ASPSM_MSGT_DOWN_ACK,	5).
-define(M2UA_ASPSM_MSGT_BEAT_ACK,	6).

-define(M2UA_ASPTM_MSGT_ACTIVE,		1).
-define(M2UA_ASPTM_MSGT_INACTIVE,	2).
-define(M2UA_ASPTM_MSGT_ACTIVE_ACK,	3).
-define(M2UA_ASPTM_MSGT_INACTIVE_ACK,	4).

-define(M2UA_MGMT_MSGT_ERROR,		0).
-define(M2UA_MGMT_MSGT_NOTIFY,		1).

-define(M2UA_MGMT_IIM_REG_REQ,		1).
-define(M2UA_MGMT_IIM_REG_RSP,		2).
-define(M2UA_MGMT_IIM_DEREG_REQ,	3).
-define(M2UA_MGMT_IIM_DEREG_RSP,	4).

% RFC 3331 Section 3.1.6 Common Parameter Tags
% 0: reserved
-define(M2UA_P_COM_INTF_ID_INT,		1).
% 2: unused
-define(M2UA_P_COM_INTF_ID_TEXT,	3).
-define(M2UA_P_COM_INFO_STRING,		4).
% 5: unused
% 6: unused
-define(M2UA_P_COM_DIAG_INFO,		7).
-define(M2UA_P_COM_INTF_ID_INT_RANGE,	8).
-define(M2UA_P_COM_HEARTB_DATA,		9).
% 10: unused
-define(M2UA_P_COM_TRAF_MODE_T,		11).
-define(M2UA_P_COM_ERR_CODE,		12).
-define(M2UA_P_COM_STATUS_T,		13).
% 14: unused
% 15: unused
% 16: unused
-define(M2UA_P_COM_ASP_ID,		17).
% 18: unused
-define(M2UA_P_COM_CORREL_ID,		19).

-define(M2UA_P_MAUP_STATE,		16#302).
-define(M2UA_P_MAUP_ACTION,		16#306).
-define(M2UA_P_MAUP_SEQN,		16#307).
-define(M2UA_P_MAUP_RESULT,		16#308).

% Section 3.3.1.5
-define(M2UA_MAUP_S_LPO_SET,		0).
-define(M2UA_MAUP_S_LPO_CLEAR,		1).
-define(M2UA_MAUP_S_EMER_SET,		2).
-define(M2UA_MAUP_S_EMER_CLEAR,		3).
-define(M2UA_MAUP_S_FLUSH_BUFFERS,	4).

% Section 3.3.1.7 State Indication
-define(M2UA_MAUP_SI_RPO_ENTER,		1).
-define(M2UA_MAUP_SI_RPO_EXIT,		2).
-define(M2UA_MAUP_SI_LPO_ENTER,		3).
-define(M2UA_MAUP_SI_LPO_EXIT,		4).


% Section 3.3.1.9 Retrieval Request
-define(M2UA_MAUP_ACT_RTRV_BSN,		1).
-define(M2UA_MAUP_ACT_RTRV_MSGS,	2).


-record(m2ua_msg, {
	msg_class	:: 0..255,
	msg_type	:: 0..255,
	parameters
	}).

