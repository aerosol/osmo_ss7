{application, osmo_ss7,
	[{description, "Osmocom SS7 code"},
	 {vsn, "1"},
	 {modules, [	osmo_util, exprecs,
			ipa_proto, 
			bssmap_codec,
			isup_codec,
			m2ua_codec,
			m3ua_codec, m3ua_core,
			mtp3_codec,
			sccp_codec, sccp_scoc,  sccp_scrc,
			osmo_ss7_gtt,
			osmo_ss7_pcap
		]},
	 {registered, []},
	 {applications, []},
	 {env, [
	  ]}
]}.
