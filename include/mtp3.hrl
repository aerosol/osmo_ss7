-define(MTP3_NETIND_INTERNATIONAL,	0).
-define(MTP3_NETIND_NATIONAL,		2).

-define(MTP3_SERV_MGMT,		0).
-define(MTP3_SERV_MTN,		1).
-define(MTP3_SERV_SCCP,		3).
-define(MTP3_SERV_TUP,		4).
-define(MTP3_SERV_ISUP,		5).

-record(mtp3_routing_label, {
	sig_link_sel,
	origin_pc,
	dest_pc
	}).

-record(mtp3_msg, {
	network_ind,
	service_ind,
	routing_label,
	m3ua_mp,		% Message Priority, only in M3UA based msg
	payload
	}).

-record(mtp3mg_msg, {
	h0,
	h1,
	payload
	}).

-define(MTP3MG_H0_TEST, 1).

-define(MTP3MG_H1_SLTM,	1).
-define(MTP3MG_H1_SLTA,	2).
