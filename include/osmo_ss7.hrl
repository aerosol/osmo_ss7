
-record(sigtran_peer, {
	ip,
	port,
	point_code
}).

-record(sigtran_link, {
	type,
	name,
	linkset_name,
	sls,
	local,
	remote
}).


