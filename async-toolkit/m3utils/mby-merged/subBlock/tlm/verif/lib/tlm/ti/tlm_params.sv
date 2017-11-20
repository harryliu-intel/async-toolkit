/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_params.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 defines file
 
 here all IP define are declarted
 
 IOSF interface defins, IP SB ports defines .....
 

 
 

*/
// MAX_DATA_LEN  according to req_dlen
// MMAX_ADDR     according to maddress
// TMAX_ADDR     according to taddress
// AGENT_WIDTH   according to req_agent (not used by tlm)
// MNUMCHAN      number of master channels (VC0, VC1, VCp) (0->one channel, 1->two channels)
// TNUMCHAN      number of target channels (VC0, VC1, VCp) (0->one channel, 1->two channels)
// MNUMCHANL2    roundup(log2(MNUMCHAN+1) - 1
// TNUMCHANL2    roundup(log2(TNUMCHAN+1) - 1
// MD_WIDTH      according to mdata 
// TD_WIDTH      according to tdata
// DEST_ID_WIDTH according to mdest_id, tdest_if, req_dest_id

`define TLM1_MAX_DATA_LEN   9
`define TLM1_MMAX_ADDR      35
`define TLM1_TMAX_ADDR      35
`define TLM1_AGENT_WIDTH    2
`define TLM1_MNUMCHAN       1
`define TLM1_TNUMCHAN       1
`define TLM1_MNUMCHANL2     0
`define TLM1_TNUMCHANL2     0
`define TLM1_MD_WIDTH       63
`define TLM1_TD_WIDTH       63
`define TLM1_DEST_ID_WIDTH  7
`define TLM1_SRC_ID_WIDTH  7
`define TLM1_MSAI_WIDTH     6
`define TLM1_TSAI_WIDTH     6

`define TLM1_IOSF_PRI_PARAMS \
        .MAX_DATA_LEN   (`TLM1_MAX_DATA_LEN),\
        .MMAX_ADDR      (`TLM1_MMAX_ADDR),\
        .TMAX_ADDR      (`TLM1_TMAX_ADDR),\
        .AGENT_WIDTH    (`TLM1_AGENT_WIDTH),\
        .MNUMCHAN       (`TLM1_MNUMCHAN),\
        .TNUMCHAN       (`TLM1_TNUMCHAN),\
        .MNUMCHANL2     (`TLM1_MNUMCHANL2),\
        .TNUMCHANL2     (`TLM1_TNUMCHANL2),\
        .MD_WIDTH       (`TLM1_MD_WIDTH),\
        .TD_WIDTH       (`TLM1_TD_WIDTH),\
        .DEST_ID_WIDTH  (`TLM1_DEST_ID_WIDTH),\
        .SRC_ID_WIDTH   (`TLM1_SRC_ID_WIDTH),\
        .MSAI_WIDTH     (`TLM1_MSAI_WIDTH),\
        .TSAI_WIDTH     (`TLM1_TSAI_WIDTH)




`define TLM1_IOSF_SB_PARAMS \
        .PAYLOAD_WIDTH(8),\
        .AGENT_MASTERING_SB_IF(0)

	  // INTEG - need to replace this define with TLM1 IP real values
`define TLM1_RDY_FOR_RST_OPCODE 8'hd0
`define TLM1_PME_OPCODE 8'ha7

`define TLM1_SB_PORT_ID 8'hAA
`define TLM1_FUSE_PULL_EP_ID 8'h40
`define TLM1_PMC_EP_ID 8'h52
`define TLM1_DFX_EP_ID 8'h58
`define TLM1_HOST_EP_ID 8'h8

`define TLM1_PMC_SAI 8'h33

// Params for Chassis Reset PKG - need to replace this define with TLM1 IP real values
`define TLM1_CCU_NUM_SLICES 1
`define TLM1_CCU_VC_PARAMS \
    .NUM_SLICES(`TLM1_CCU_NUM_SLICES)\

`define TLM1_NO_FAB_PGCB 1
`define TLM1_NUM_SB_EP 1
`define TLM1_NUM_PRIM_EP 1
`define TLM1_NUM_PMC_WAKE 1
`define TLM1_NUM_SIP_PGCB 
`define TLM1_NUM_FET 1
`define TLM1_NUM_SW_REQ 1
`define TLM1_NUM_FAB_PGCB 1
`define TLM1_NUM_D3 1
`define TLM1_NUM_D0I3 1
`define TLM1_NO_SIP_PGCB 0
`define TLM1_NO_PRIM_EP 0

`define TLM1_CHASSIS_PWRGATE_CCAGENT_PARAMS \
    .NO_FAB_PGCB(`TLM1_NO_FAB_PGCB),\
    .NUM_SB_EP(`TLM1_NUM_SB_EP),\
    .NUM_PRIM_EP(`TLM1_NUM_PRIM_EP),\
    .NUM_PMC_WAKE(`TLM1_NUM_PMC_WAKE),\
    .NUM_SIP_PGCB(`TLM1_NUM_SIP_PGCB),\
    .NUM_FET(`TLM1_NUM_FET),\
    .NUM_SW_REQ(`TLM1_NUM_SW_REQ),\
    .NUM_FAB_PGCB(`TLM1_NUM_FAB_PGCB),\
    .NUM_D3(`TLM1_NUM_D3),\
    .NUM_D0I3(`TLM1_NUM_D0I3),\
    .NO_SIP_PGCB(`TLM1_NO_SIP_PGCB),\
    .NO_PRIM_EP(`TLM1_NO_PRIM_EP)

