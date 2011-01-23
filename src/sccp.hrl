
% Table 1 / Q.713 - SCCP Message Types
-define(SCCP_MSGT_CR,		1).	% Connection request
-define(SCCP_MSGT_CC,		2).	% Connection confirm
-define(SCCP_MSGT_CREF,		3).	% Connection refused
-define(SCCP_MSGT_RLSD,		4).	% Released
-define(SCCP_MSGT_RLC,		5).	% Release complete
-define(SCCP_MSGT_DT1,		6).	% Data form 1
-define(SCCP_MSGT_DT2,		7).	% Data form 2
-define(SCCP_MSGT_AK,		8).	% Data acknowledgement
-define(SCCP_MSGT_UDT,		9).	% Unitdata
-define(SCCP_MSGT_UDTS,		10).	% Unitdata service
-define(SCCP_MSGT_ED,		11).	% Expedited data
-define(SCCP_MSGT_EA,		12).	% Expedited data ack
-define(SCCP_MSGT_RSR,		13).	% Reset Request
-define(SCCP_MSGT_RSC,		14).	% Reset Confirmation
-define(SCCP_MSGT_ERR,		15).	% Protocol data unit error
-define(SCCP_MSGT_IT,		16).	% Inactivity test
-define(SCCP_MSGT_XUDT,		17).	% Extended unitdata
-define(SCCP_MSGT_XUDTS,	18).	% Extended unitdata service
-define(SCCP_MSGT_LUDT,		19).	% Long unitdata
-define(SCCP_MSGT_LUDTS,	20).	% Long unitdata service

% Table 2 / Q.713 - SCCP parameter name codes
-define(SCCP_PNC_END_OF_OPTIONAL,		0).
-define(SCCP_PNC_DESTINATION_LOCAL_REFERENCE,	1).
-define(SCCP_PNC_SOURCE_LOCAL_REFERENCE,	2).
-define(SCCP_PNC_CALLED_PARTY_ADDRESS,		3).
-define(SCCP_PNC_CALLING_PARTY_ADDRESS,		4).
-define(SCCP_PNC_PROTOCOL_CLASS,		5).
-define(SCCP_PNC_SEGMENTING,			6).
-define(SCCP_PNC_RECEIVE_SEQ_NUMBER,		7).
-define(SCCP_PNC_SEQUENCING,			8).
-define(SCCP_PNC_CREDIT,			9).
-define(SCCP_PNC_RELEASE_CAUSE,			10).
-define(SCCP_PNC_RETURN_CAUSE,			11).
-define(SCCP_PNC_RESET_CAUSE,			12).
-define(SCCP_PNC_ERROR_CAUSE,			13).
-define(SCCP_PNC_REFUSAL_CAUSE,			14).
-define(SCCP_PNC_DATA,				15).
-define(SCCP_PNC_SEGMENTATION,			16).
-define(SCCP_PNC_HOP_COUNTER,			17).
-define(SCCP_PNC_IMPORTANCE,			18).
-define(SCCP_PNC_LONG_DATA,			19).

% According to Q.713 Section 3.4.1
-define(SCCP_GTI_NO_GT,		2#0000).
-define(SCCP_GTI_NAT_ONLY,	2#0001).
-define(SCCP_GTI_TT_ONLY,	2#0010).
-define(SCCP_GTI_TT_NP_ENC,	2#0011).
-define(SCCP_GTI_TT_NP_ENC_NAT,	2#0100).

% According to Q.731 Section 3.4.2.2
-define(SCCP_SSN_UNKNOWN,	2#000000000).
-define(SCCP_SSN_SCCP_MGMT,	2#000000001).
-define(SCCP_SSN_ITU_T,		2#000000010).
-define(SCCP_SSN_ISUP,		2#000000011).
-define(SCCP_SSN_OAM,		2#000000100).
-define(SCCP_SSN_MAP,		2#000000101).
-define(SCCP_SSN_HLR,		2#000000110).
-define(SCCP_SSN_VLR,		2#000000111).
-define(SCCP_SSN_MSC,		2#000001000).
-define(SCCP_SSN_EIR,		2#000001001).
-define(SCCP_SSN_AUC,		2#000001010).
-define(SCCP_SSN_ISDN_SS,	2#000001011).
-define(SCCP_SSN_RES_NAT,	2#000001100).
-define(SCCP_SSN_BISDN,		2#000001101).
-define(SCCP_SSN_TC_TEST,	2#000001110).



% a single parsed SCCP message
-record(sccp_msg, {
	msg_type,
	parameters
	}).


% a primitive how it is used inside the SCCP stack and to the user
-record(primitive, {
	  subsystem,
	  gen_name,
	  spec_name,
	  parameters
	}).
