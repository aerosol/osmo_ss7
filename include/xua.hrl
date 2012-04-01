
-record(xua_msg, {
	 version	:: 0..255,
	 msg_class	:: 0..255,
	 msg_type	:: 0..255,
	 payload
	}).
