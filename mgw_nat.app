{application, mgw_nat,
	[{description, "Media Gateway NAT"},
	 {vsn, "1"},
	 {modules, [mgw_nat_app, mgw_nat_sup, mgw_nat_usr, mgw_nat]},
	 {mod, {mgw_nat_app, []}},
	 {env, [
		% SCCP rewrite
		{real_hlr_gt, [6,3,9,1,8,0,0,0,4,0,1,2]},
		{nat_hlr_gt, [3,5,4,8,9,0,0,0,7,1]},

		% ISUP rewrite
		{msrn_pfx_msc, [3,5,4,8,9,0,9,9]},
		{msrn_pfx_stp, [6,3,9,2,9,9,4,2,0,0]},
		{intern_pfx, [6,3]},

		% SCTP / IP config
		{msc_local_ip, any},
		{msc_local_port, 2904},
		{msc_remote_ip, {172,16,1,81}},
		{stp_remote_ip, {172,16,249,20}},
		{stp_remote_port, 2904}
	  ]}
]}.
