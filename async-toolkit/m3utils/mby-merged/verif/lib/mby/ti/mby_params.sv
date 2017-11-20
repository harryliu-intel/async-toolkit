/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_params.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY defines file
 
 here all IP define are declarted
 
 IOSF interface defins, IP SB ports defines .....
 

 
 

*/
// MAX_DATA_LEN  according to req_dlen
// MMAX_ADDR     according to maddress
// TMAX_ADDR     according to taddress
// AGENT_WIDTH   according to req_agent (not used by mby)
// MNUMCHAN      number of master channels (VC0, VC1, VCp) (0->one channel, 1->two channels)
// TNUMCHAN      number of target channels (VC0, VC1, VCp) (0->one channel, 1->two channels)
// MNUMCHANL2    roundup(log2(MNUMCHAN+1) - 1
// TNUMCHANL2    roundup(log2(TNUMCHAN+1) - 1
// MD_WIDTH      according to mdata 
// TD_WIDTH      according to tdata
// DEST_ID_WIDTH according to mdest_id, tdest_if, req_dest_id

`define MBY_MAX_DATA_LEN   9
`define MBY_MMAX_ADDR      35
`define MBY_TMAX_ADDR      35
`define MBY_AGENT_WIDTH    2
`define MBY_MNUMCHAN       1
`define MBY_TNUMCHAN       1
`define MBY_MNUMCHANL2     0
`define MBY_TNUMCHANL2     0
`define MBY_MD_WIDTH       63
`define MBY_TD_WIDTH       63
`define MBY_DEST_ID_WIDTH  7
`define MBY_SRC_ID_WIDTH  7
`define MBY_MSAI_WIDTH     6
`define MBY_TSAI_WIDTH     6

`define MBY_IOSF_PRI_PARAMS \
        .MAX_DATA_LEN   (`MBY_MAX_DATA_LEN),\
        .MMAX_ADDR      (`MBY_MMAX_ADDR),\
        .TMAX_ADDR      (`MBY_TMAX_ADDR),\
        .AGENT_WIDTH    (`MBY_AGENT_WIDTH),\
        .MNUMCHAN       (`MBY_MNUMCHAN),\
        .TNUMCHAN       (`MBY_TNUMCHAN),\
        .MNUMCHANL2     (`MBY_MNUMCHANL2),\
        .TNUMCHANL2     (`MBY_TNUMCHANL2),\
        .MD_WIDTH       (`MBY_MD_WIDTH),\
        .TD_WIDTH       (`MBY_TD_WIDTH),\
        .DEST_ID_WIDTH  (`MBY_DEST_ID_WIDTH),\
        .SRC_ID_WIDTH   (`MBY_SRC_ID_WIDTH),\
        .MSAI_WIDTH     (`MBY_MSAI_WIDTH),\
        .TSAI_WIDTH     (`MBY_TSAI_WIDTH)




`define MBY_IOSF_SB_PARAMS \
        .PAYLOAD_WIDTH(8),\
        .AGENT_MASTERING_SB_IF(0)

	  // INTEG - need to replace this define with MBY IP real values
`define MBY_RDY_FOR_RST_OPCODE 8'hd0
`define MBY_PME_OPCODE 8'ha7

`define MBY_SB_PORT_ID 8'hAA
`define MBY_FUSE_PULL_EP_ID 8'h40
`define MBY_PMC_EP_ID 8'h52
`define MBY_DFX_EP_ID 8'h58
`define MBY_HOST_EP_ID 8'h8

`define MBY_PMC_SAI 8'h33

// Params for Chassis Reset PKG - need to replace this define with MBY IP real values
`define MBY_CCU_NUM_SLICES 1
`define MBY_CCU_VC_PARAMS \
    .NUM_SLICES(`MBY_CCU_NUM_SLICES)\

`define MBY_NO_FAB_PGCB 1
`define MBY_NUM_SB_EP 1
`define MBY_NUM_PRIM_EP 1
`define MBY_NUM_PMC_WAKE 1
`define MBY_NUM_SIP_PGCB 
`define MBY_NUM_FET 1
`define MBY_NUM_SW_REQ 1
`define MBY_NUM_FAB_PGCB 1
`define MBY_NUM_D3 1
`define MBY_NUM_D0I3 1
`define MBY_NO_SIP_PGCB 0
`define MBY_NO_PRIM_EP 0

`define MBY_CHASSIS_PWRGATE_CCAGENT_PARAMS \
    .NO_FAB_PGCB(`MBY_NO_FAB_PGCB),\
    .NUM_SB_EP(`MBY_NUM_SB_EP),\
    .NUM_PRIM_EP(`MBY_NUM_PRIM_EP),\
    .NUM_PMC_WAKE(`MBY_NUM_PMC_WAKE),\
    .NUM_SIP_PGCB(`MBY_NUM_SIP_PGCB),\
    .NUM_FET(`MBY_NUM_FET),\
    .NUM_SW_REQ(`MBY_NUM_SW_REQ),\
    .NUM_FAB_PGCB(`MBY_NUM_FAB_PGCB),\
    .NUM_D3(`MBY_NUM_D3),\
    .NUM_D0I3(`MBY_NUM_D0I3),\
    .NO_SIP_PGCB(`MBY_NO_SIP_PGCB),\
    .NO_PRIM_EP(`MBY_NO_PRIM_EP)

