// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) with the Shared Memory Mesh Write Arbiter

interface egr_smm_writearb_if ();
  import mby_egr_pkg::*;
    
    logic [W_SEG_PTR-1:0]            wreq_seg_ptr;
    logic [W_SEMA-1:0]                  wreq_sema;
    logic [W_WD_SEL-1:0]              wreq_wd_sel;
    logic [W_REQ_ID-1:0]              wreq_req_id;
    logic [W_WORD_BITS-1:0]             wreq_data;
    logic                              wreq_valid;
    logic [W_XACT_CREDITS-1:0]       wreq_credits;
    
modport egr(
    output wreq_seg_ptr,
    output    wreq_sema,
    output  wreq_wd_sel,
    output  wreq_req_id,
    output    wreq_data,
    output   wreq_valid,
    input  wreq_credits 
    );

modport smm_writearb(
    input  wreq_seg_ptr,
    input     wreq_sema,
    input   wreq_wd_sel,
    input   wreq_req_id,
    input     wreq_data,
    input    wreq_valid,
    output wreq_credits 

    );
    
    
    
endinterface : egr_smm_writearb_if