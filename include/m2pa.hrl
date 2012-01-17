% RFC 4165 MTP2 P2P Adaption Layer coding / decoding

% (C) 2012 by Harald Welte <laforge@gnumonks.org>
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

-define(M2PA_PPID,		5).
-define(M2PA_PORT,		3565).

% Section 2.1.3
-define(M2PA_CLASS_M2PA,	11).

% Section 2.1.4
-define(M2PA_TYPE_USER,		1).
-define(M2PA_TYPE_LINK,		2).

% Section 2.3.2
-define(M2PA_LS_ALIGNMENT,	1).
-define(M2PA_LS_PROVING_NORMAL,	2).
-define(M2PA_LS_PROVING_EMERG,	3).
-define(M2PA_LS_READY,		4).
-define(M2PA_LS_PROC_OUTAGE,	5).
-define(M2PA_LS_PROC_RECOVERED,	6).
-define(M2PA_LS_BUSY,		7).
-define(M2PA_LS_BUSY_ENDED,	8).
-define(M2PA_LS_OOS,		9).

% SCTP stream IDs
-define(M2PA_STREAM_STATUS,	0).
-define(M2PA_STREAM_USER,	1).

-record(m2pa_msg, {
		msg_class,
		msg_type,
		fwd_seq_nr,
		back_seq_nr,
		mtp3,
		parameters
	}).
