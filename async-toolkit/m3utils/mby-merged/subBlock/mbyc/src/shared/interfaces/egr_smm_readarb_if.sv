// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) with the Shared Memory Mesh Read Arbiter

interface egr_smm_readarb_if ();
  import mby_egr_pkg::*;
    
//    rreq_t                 rreq;
//    rreq_credits_t rreq_credits;
//    rreq_valid_t     rreq_valid;
//    
//    rrsp_t                 rrsp;
//    rrsp_valid_t     rrsp_valid;

    logic [W_SEG_PTR-1:0]            rreq_seg_ptr;
    logic [W_SEMA-1:0]                  rreq_sema;
    logic [W_WD_SEL-1:0]              rreq_wd_sel;
    logic [W_REQ_ID-1:0]              rreq_req_id;
    logic                              rreq_valid;
    logic [W_XACT_CREDITS-1:0]       rreq_credits;
    
    logic [W_RRSP_DEST_BLOCK-1:0] rrsp_dest_block;
    logic [W_REQ_ID-1:0]              rrsp_req_id;
    logic [W_WORD_BITS-1:0]             rrsp_data;
    logic                              rrsp_valid;
    
modport egr(
    output    rreq_seg_ptr,
    output       rreq_sema,
    output     rreq_wd_sel,
    output     rreq_req_id,
    output      rreq_valid,
    input     rreq_credits,
    
    input  rrsp_dest_block,
    input      rrsp_req_id,
    input        rrsp_data,
    input       rrsp_valid    
    );

modport smm_readarb(
    input      rreq_seg_ptr,
    input         rreq_sema,
    input       rreq_wd_sel,
    input       rreq_req_id,
    input        rreq_valid,
    output     rreq_credits,
    
    output  rrsp_dest_block,
    output      rrsp_req_id,
    output        rrsp_data,
    output       rrsp_valid    
    );
    
    
    
endinterface : egr_smm_readarb_if
