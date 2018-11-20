// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) with the MultiCast Shared Table

interface egr_mc_table_if ();
  import mby_egr_pkg::*;
    
//    mcsharedtablememory_table_req_t mcsharedtablememory_table_req;
//    mcsharedtablememory_table_rsp_t mcsharedtablememory_table_rsp;
//    mcsharedtablememory_table_ack_t mcsharedtablememory_table_ack;    
   logic mcsharedtablememory_table_req;
   logic mcsharedtablememory_table_rsp;
   logic mcsharedtablememory_table_ack;    
    
modport egr(
    output mcsharedtablememory_table_req,
    input  mcsharedtablememory_table_rsp,
    input  mcsharedtablememory_table_ack
    );

modport mc_table(
    input   mcsharedtablememory_table_req,
    output  mcsharedtablememory_table_rsp,
    output  mcsharedtablememory_table_ack
    );
    
    
    
endinterface : egr_mc_table_if