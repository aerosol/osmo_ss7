{application, mgw_nat,
	[{description, "Media Gateway NAT"},
	 {vsn, "1"},
	 {modules, [mgw_nat_app, mgw_nat_sup, mgw_nat_usr, mgw_nat]},
	 {mod, {mgw_nat_app, []}},
	 {env, [
		% SCCP rewrite
		{sccp_rewrite_tbl, [
			{ 12340000, 98760000, "HLR" },
			{ 12340001, 98760001, "VLR" }
		]},

		% ISUP rewrite
		{msrn_pfx_msc, 35489099},
		{msrn_pfx_stp, 6392994200},
		{intern_pfx, 63},

		% SCTP / IP config
		{msc_local_ip, any},
		{msc_local_port, 2904},
		{msc_remote_ip, {172,16,1,81}},
		{stp_remote_ip, {172,16,249,20}},
		{stp_remote_port, 2904}
	  ]}
]}.
