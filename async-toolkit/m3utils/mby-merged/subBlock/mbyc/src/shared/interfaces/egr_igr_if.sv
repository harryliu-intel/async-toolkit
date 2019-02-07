// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) with Ingress (IGR) for sharing clean/dirty pod read/write requests

interface egr_igr_if ();
  import mby_egr_pkg::*;
    
    
  // Dirty pointer Read port
  logic                   egr_igr_wreq_valid; 
  logic [W_SEG_PTR-1:0]   egr_igr_wr_seg_ptr; //[19:0]
  logic [W_SEMA-1:0]      egr_igr_wr_sema;    //[ 3:0]
  logic [W_WD_SEL-1:0]    egr_igr_wr_wd_sel;  //[ 2:0]
  logic [W_REQ_ID-1:0]    egr_igr_wreq_id;    //[12:0]
  logic [W_WORD_BITS-1:0] egr_igr_wr_data;    // 64*8
  
// Assume IGR has some kind of ACK for buffer space  
  logic [W_XACT_CREDITS-1:0] igr_egr_wreq_credits; // temp value
  
  // Pointer cache read/rsp

  logic                    igr_egr_rreq_valid;
  logic [W_SEG_PTR-1:0]    igr_egr_seg_ptr;      //[19:0]
  logic [W_SEMA-1:0]       igr_egr_sema;         //[ 3:0]
  logic [W_WD_SEL-1:0]     igr_egr_wd_sel;       //[ 2:0]
  logic [W_REQ_ID-1:0]     igr_egr_req_id;       //[12:0]
  
  logic [W_XACT_CREDITS-1:0] egr_igr_rreq_credits; // temp value   
  
  
  logic                         egr_igr_rrsp_valid;
  logic [W_RRSP_DEST_BLOCK-1:0] egr_igr_rrsp_dest_block;  //[2:0]
  logic [W_REQ_ID-1:0]          egr_igr_rrsp_req_id;      //[12:0]
  logic [W_WORD_BITS-1:0]       egr_igr_rd_data;          //64 x 8

  //TODO Define PFC signals
    
modport egr(
    output egr_igr_wreq_valid,
    output egr_igr_wr_seg_ptr,
    output egr_igr_wr_sema,   
    output egr_igr_wr_wd_sel, 
    output egr_igr_wreq_id,
    output egr_igr_wr_data,  
    input igr_egr_wreq_credits,
    input igr_egr_rreq_valid,
    input igr_egr_seg_ptr,
    input igr_egr_sema,
    input igr_egr_wd_sel,
    input igr_egr_req_id,
    output egr_igr_rreq_credits,
    output egr_igr_rrsp_valid,
    output egr_igr_rrsp_dest_block,
    output egr_igr_rrsp_req_id,    
    output egr_igr_rd_data        
    );

modport igr(
    input egr_igr_wreq_valid,
    input egr_igr_wr_seg_ptr,
    input egr_igr_wr_sema,   
    input egr_igr_wr_wd_sel, 
    input egr_igr_wreq_id,   
    input egr_igr_wr_data,  
    output igr_egr_wreq_credits,
    output igr_egr_rreq_valid,
    output igr_egr_seg_ptr,
    output igr_egr_sema,
    output igr_egr_wd_sel,
    output igr_egr_req_id,
    input egr_igr_rreq_credits,
    input egr_igr_rrsp_valid,
    input egr_igr_rrsp_dest_block,
    input egr_igr_rrsp_req_id,    
    input egr_igr_rd_data        
    );

endinterface : egr_igr_if
