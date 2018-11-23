// -- Description  : This is the interface for connecting 
//                   Egress (EGR) to the CSR using AHB and DFx buses 

interface egr_ahb_if ();
  import mby_egr_pkg::*;
    
//    ahb_req_p_t         ahb_req_p; 
//    ahb_addr_t           ahb_addr; 
//    ahb_wr_t               ahb_wr; 
//    ahb_wr_data_t     ahb_wr_data; 
//    ahb_ack_p_t         ahb_ack_p; 
//    ahb_rd_data_t     ahb_rd_data; 
//    dfxsignal_in_t   dfxsignal_in;
//    dfxsignal_out_t dfxsignal_out;
    
    logic                    ahb_req_p; 
    logic [W_AHB_ADDR-1:0]    ahb_addr; 
    logic                       ahb_wr; 
    logic [W_AHB_DATA-1:0] ahb_wr_data; 
    logic                    ahb_ack_p; 
    logic [W_AHB_DATA-1:0] ahb_rd_data; 
    logic                 dfxsignal_in; //FIXME Needs to be defined
    logic                dfxsignal_out; //FIXME Needs to be defined
    
modport egr(
    input      ahb_req_p,
    input       ahb_addr,
    input         ahb_wr, 
    input    ahb_wr_data,
    output     ahb_ack_p, 
    output   ahb_rd_data,
    input   dfxsignal_in,
    output dfxsignal_out
    );

modport ahb(
    output    ahb_req_p,
    output     ahb_addr,
    output       ahb_wr, 
    output  ahb_wr_data,
    input     ahb_ack_p, 
    input   ahb_rd_data
    );
    
modport dfx(
    output dfxsignal_in,
    input dfxsignal_out
    );

    
endinterface : egr_ahb_if