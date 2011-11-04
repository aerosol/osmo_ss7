% Table 1 / Q.762 - ISDN user part message acronyms
% Table C-3 / Q.762
-define(ISUP_MSGT_ACM, 2#00000110).	%  Address complete
-define(ISUP_MSGT_ANM, 2#00001001).	%  Answer
%-define(ISUP_MSGT_APM).	%  Application transport
-define(ISUP_MSGT_BLA, 2#00010101).	%  Blocking acknowledgement
-define(ISUP_MSGT_BLO, 2#00010011).	%  Blocking
-define(ISUP_MSGT_CCR, 2#00010001).	%  Continuity check request
-define(ISUP_MSGT_CFN, 2#00101111).	%  Confusion
-define(ISUP_MSGT_CGB, 2#00011000).	%  Circuit group blocking
-define(ISUP_MSGT_CGBA,2#00011010).%  Circuit group blocking acknowledgement
-define(ISUP_MSGT_CGU, 2#00011001).	%  Circuit group unblocking
-define(ISUP_MSGT_CGUA,2#00011011).%  Circuit group unblocking acknowledgement
-define(ISUP_MSGT_CON, 2#00000111).	%  Connect
-define(ISUP_MSGT_COT, 2#00000101).	%  Continuity
-define(ISUP_MSGT_CPG, 2#00101100).	%  Call progress
-define(ISUP_MSGT_CRG, 2#00110001).	%  Charge information
-define(ISUP_MSGT_CQM, 2#00101010).	%  Circuit group query
-define(ISUP_MSGT_CQR, 2#00101011).	%  Circuit group query response
-define(ISUP_MSGT_DRS, 2#00100111).	%  Delayed release (reserved – used in 1988 version)
-define(ISUP_MSGT_FAA, 2#00100000).	%  Facility accepted
%-define(ISUP_MSGT_FAC).	%  Facility
-define(ISUP_MSGT_FAR, 2#00011111).	%  Facility request
-define(ISUP_MSGT_FOT, 2#00001000).	%  Forward transfer
-define(ISUP_MSGT_FRJ, 2#00100001).	%  Facility reject
-define(ISUP_MSGT_GRA, 2#00101001).	%  Circuit group reset acknowledgement
-define(ISUP_MSGT_GRS, 2#00010111).	%  Circuit group reset
-define(ISUP_MSGT_IAM, 2#00000001).	%  Initial address
%-define(ISUP_MSGT_IDR).	%  Identification request
%-define(ISUP_MSGT_IRS).	%  Identification response
-define(ISUP_MSGT_INF, 2#00000100).	%  Information
-define(ISUP_MSGT_INR, 2#00000011).	%  Information request
-define(ISUP_MSGT_LPA, 2#00100100).	%  Loop back acknowledgement
%-define(ISUP_MSGT_LOP).	%  Loop prevention
%-define(ISUP_MSGT_NRM).	%  Network resource management
-define(ISUP_MSGT_OLM, 2#00110000).	%  Overload
-define(ISUP_MSGT_PAM, 2#00101000).	%  Pass-along
%-define(ISUP_MSGT_PRI).	%  Pre-release information
-define(ISUP_MSGT_REL, 2#00001100).	%  Release
-define(ISUP_MSGT_RES, 2#00001110).	%  Resume
-define(ISUP_MSGT_RLC, 2#00010000).	%  Release complete
-define(ISUP_MSGT_RSC, 2#00010010).	%  Reset circuit
-define(ISUP_MSGT_SAM, 2#00000010).	%  Subsequent address
%-define(ISUP_MSGT_SDM).	%  Subsequent directory number
%-define(ISUP_MSGT_SGM).	%  Segmentation
-define(ISUP_MSGT_SUS, 2#00001101).	%  Suspend
-define(ISUP_MSGT_UBL, 2#00010100).	%  Unblocking
-define(ISUP_MSGT_UBA, 2#00010110).	%  Unblocking acknowledgement
-define(ISUP_MSGT_UCIC,2#00101110).%  Unequipped circuit identification code
%-define(ISUP_MSGT_UPA).	%  User part available
%-define(ISUP_MSGT_UPT).	%  User part test
-define(ISUP_MSGT_USR, 2#00101101).	%  User-to-user information


% TABLE C-4/Q.767
-define(ISUP_PAR_ACC_TRANSP,		2#00000011).	% Access transport
-define(ISUP_PAR_AUT_CONG_LVL,		2#00100111).	% Automatic congestion level
-define(ISUP_PAR_BACKW_CALL_IND,	2#00010001).	% Backward call indicators
-define(ISUP_PAR_CALL_MOD_IND,		2#00010111).	% Call modification indicators
-define(ISUP_PAR_CALL_REF,		2#00000001).	% Call reference
-define(ISUP_PAR_CALLED_P_NUM,		2#00000100).	% Called party number
-define(ISUP_PAR_CALLING_P_NUM,		2#00001010).	% Calling party number
-define(ISUP_PAR_CALLING_P_CAT,		2#00001001).	% Calling party category
-define(ISUP_PAR_CAUSE_IND,		2#00010010).	% Cause indicators
-define(ISUP_PAR_CG_SUB_MSGT_IND,	2#00010101).	% Circuit group supervision message type indicator
-define(ISUP_PAR_CIRC_STATE_IND,	2#00100110).	% Circuit state indicator
-define(ISUP_PAR_CLSD_U_INTERL_CODE,	2#00011010).	% Closed user interlock code
-define(ISUP_PAR_CONNECTED_NUM,		2#00100001).	% Connected number
-define(ISUP_PAR_CONN_REQ,		2#00001101).	% Connection request
-define(ISUP_PAR_CONTINUITY_IND,	2#00010000).	% Continuity idnicators
-define(ISUP_PAR_END_OF_OPT,		2#00000000).	% End of optional parameters
-define(ISUP_PAR_EVENT_INFO,		2#00100100).	% Event information
-define(ISUP_PAR_FACILITY_IND,		2#00011000).	% Facility indicators
-define(ISUP_PAR_FW_CALL_IND,		2#00000111).	% Forward call indicators
-define(ISUP_PAR_INFO_IND,		2#00001111).	% Information indicators
-define(ISUP_PAR_INFO_REQ_IND,		2#00001110).	% Information request indicatos
-define(ISUP_PAR_NAT_OF_CONN_IND,	2#00000110).	% Nature of connection indicators
-define(ISUP_PAR_OPT_BW_CALL_IND,	2#00101001).	% Optional backward call indicators
-define(ISUP_PAR_OPT_FW_CALL_IND,	2#00001000).	% Optional forward call indicators
-define(ISUP_PAR_ORIG_CALLED_NUM,	2#00101000).	% Original called number
-define(ISUP_PAR_RANGE_AND_STATUS,	2#00010110).	% Range and status
-define(ISUP_PAR_REDIR_NUM,		2#00001011).	% Redirecting number
-define(ISUP_PAR_REDIR_INFO,		2#00010011).	% Redirection information
-define(ISUP_PAR_REDIRECTION_NUM,	2#00001100).	% Redirection number
-define(ISUP_PAR_SIGN_POINT_CODE,	2#00011110).	% Signalling point code
-define(ISUP_PAR_SUBSEQ_NUM,		2#00000101).	% Subsequent number
-define(ISUP_PAR_SUSP_RES_IND,		2#00100010).	% Suspend/resume indicators
-define(ISUP_PAR_XMIT_NET_SEL,		2#00100011).	% Transmit network selection
-define(ISUP_PAR_TRANSM_MED_REQ,	2#00000010).	% Transmission medium requirement
-define(ISUP_PAR_USER_SERV_INFO,	2#00011101).	% User service information
-define(ISUP_PAR_USER_USER_IND,		2#00101010).	% User-to-user indicators
-define(ISUP_PAR_USER_USER_INFO,	2#00100000).	% User-to-user information

-define(ISUP_ADDR_NAT_SUBSCRIBER,	2#0000001).	% Subscriber number
-define(ISUP_ADDR_NAT_NATIONAL,		2#0000011).	% National (significant) number
-define(ISUP_ADDR_NAT_INTERNATIONAL,	2#0000100).	% International number

-record(party_number, {
	nature_of_addr_ind,
	internal_net_num,	% only in called party
	number_incompl_ind,	% only in calling party
	numbering_plan,
	present_restrict,	% only in calling party
	screening_ind,		% only in calling party
	phone_number}
	).

-type isup_cic()	::	0..65535.

-record(isup_msg, {
	msg_type		:: non_neg_integer(),
	cic			:: isup_cic(),
	parameters		:: list()
	}).
