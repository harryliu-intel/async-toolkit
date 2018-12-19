

//`include "mby_msh_defines.vh";                                  // include file with `defines 

module tb_mby_mim
import mby_msh_pkg::*,shared_pkg::*;                                         

();



// cclk period 1000/833:
logic clk;
always #1 clk = ~clk;

logic reset;

initial begin
    clk         = 1'b0;
    reset       = 1'b0;

    @(posedge clk); #1;

    reset = 1'b1;

    @(posedge clk); #10;

    reset = 1'b0;
   
    @(posedge clk); #100;


    $finish;
end

msh_row_wr_req_t  wreq   [2:0];

msh_data_t        wrdata [2:0]; 

msh_row_rd_req_t  rreq   [2:0];

msh_row_rd_rsp_t  rrsp   [1:0];

msh_data_t        rddata [1:0];


integer i;


initial begin

  wreq[0].vld      = 0; 
  wreq[0].node_col = 0; 
  wreq[0].node_row = 0; 
  wreq[0].csr      = 0; 
  wreq[0].addr     = 0;
  wreq[0].age      = 1;
  wrdata[0]        = 0;
  
  rreq[0].vld      = 0;
  rreq[0].id       = 0;
  rreq[0].node_col = 0;
  rreq[0].node_row = 0;
  rreq[0].csr      = 0;
  rreq[0].addr     = 0;
  rreq[0].sema_vld = 0;
  rreq[0].sema_val = 0;
  rreq[0].age      = 0;

  rrsp[0].vld      = 0;
  rrsp[0].id       = 0;
  rddata[0]        = 0;
  
  for (i=0; i<20; i=i+1) begin     
    @(posedge clk);
  end
  
  /////////////////////////////////
  ///WRITE REQUEST
  /////////////////////////////////  

  @(posedge clk); #0.1;  
  
  wreq[0].vld   = 1; 
  wreq[0].addr  = 20'h12345;
  wrdata[0]     = 512'h11111111_22222222_33333333_44444444_55555555_66666666_77777777_88888888_99999999_10101010_aaaaaaaa_bbbbbbbb_cccccccc_dddddddd_eeeeeeee_ffffffff;
  
  @(posedge clk); #0.1;

  wreq[0].vld   = 1; 
  wreq[0].addr  = 20'h67890;
  wrdata[0]     = 512'hffffffff_eeeeeeee_dddddddd_cccccccc_bbbbbbbb_aaaaaaaa_10101010_99999999_88888888_77777777_66666666_55555555_44444444_33333333_22222222_11111111;
  
  @(posedge clk); #0.1;
  
  wreq[0].vld   = 0; 
  wreq[0].addr  = 0;
  wrdata[0]     = 0;
  
  #10
  
  /////////////////////////////////
  ///READ REQUEST
  /////////////////////////////////  

  @(posedge clk); #0.1;
  
  rreq[0].vld      = 1;
  rreq[0].id       = 16'h1122;
  //rreq[0].node_col = 0;
  //rreq[0].node_row = 0;
  //rreq[0].csr      = 0;
  rreq[0].addr     = 20'hAAAAA;
  //rreq[0].sema_vld = 0;
  //rreq[0].sema_val = 0;
  rreq[0].age      = 2;
  
  @(posedge clk); #0.1;
  
  rreq[0].vld      = 0;
  rreq[0].id       = 0;
  rreq[0].addr     = 0;
  rreq[0].age      = 0;  
  
  #10;  
  
  /////////////////////////////////
  ///READ RESPONSE
  /////////////////////////////////  
  
  @(posedge clk); #0.1;
  
  rrsp[0].vld      = 1;
  rrsp[0].id       = 16'h1122;
  rddata[0]        = 512'h11111111_22222222_33333333_44444444_55555555_66666666_77777777_88888888_99999999_10101010_aaaaaaaa_bbbbbbbb_cccccccc_dddddddd_eeeeeeee_ffffffff;

  @(posedge clk); #0.1;
  
  rrsp[0].vld      = 1;
  rrsp[0].id       = 16'h1123;
  rddata[0]        = 512'hffffffff_eeeeeeee_dddddddd_cccccccc_bbbbbbbb_aaaaaaaa_10101010_99999999_88888888_77777777_66666666_55555555_44444444_33333333_22222222_11111111;

  @(posedge clk); #0.1;
  
  rrsp[0].vld      = 0;
  rrsp[0].id       = 0;
  rddata[0]        = 0;
  
  
  #100; 
  
  
end  
  
  
  
mby_mim mim

(
   .cclk                     (clk),                                                            
   .chreset                  (reset),                         
   .csreset                  (reset), 

   .mclk                     (clk),                               
   .mhreset                  (reset), 
   .msreset                  (reset),

    ////////////////////
    // MGP writes
    ////////////////////
  
//typedef struct packed {
//    logic               vld;            // this message is valid
//    msh_col_t           node_col;       // destination node column
//    msh_row_t           node_row;       // destination node row
//    logic               csr;            // 1 = CSR access
//    mshnd_addr_t        addr;           // address within mesh node memory
//    msh_trans_age_t     age;            // mesh transaction age
//} msh_row_wr_req_t;    
    
    
/* msh_row_wr_req_t */ .i_msh_wr_req       (wreq),    //[NUM_MSH_ROW_PORTS-1:0],
/* msh_data_t       */ .i_msh_wr_data      (wrdata),  //[NUM_MSH_ROW_PORTS-1:0],
                 
/* logic            */ .o_msh_wreq_credits (),     //[NUM_MSH_ROW_PORTS-1:0],
   
    ////////////////////
    // MGP rd request
    ////////////////////
    
//typedef struct packed {
//    logic               vld;            // this message is valid
//    msh_rd_id_t         id;             // read identifier
//    msh_col_t           node_col;       // destination node column
//    msh_row_t           node_row;       // destination node row
//    logic               csr;            // 1 = CSR access
//    mshnd_addr_t        addr;           // address within mesh node memory
//    logic               sema_vld;       // if valid, read should wait for write semaphore
//    logic               sema_val;       // the value of the write semaphore to wait for
//    msh_trans_age_t     age;            // mesh transaction age
//} msh_row_rd_req_t;
        
/* msh_row_rd_req_t */ .i_msh_rd_req       (rreq),     //[NUM_MSH_ROW_PORTS-1:0],
    
/* logic            */ .o_msh_rreq_credits (),     //[NUM_MSH_ROW_PORTS-1:0],
  
    ////////////////////
    // MGP rd response
    ////////////////////
    
//typedef struct packed {
//    logic           vld;                // this message is valid
//    msh_rd_id_t     id;                 // read identifier
//} msh_row_rd_rsp_t;  

/* msh_row_rd_rsp_t */ .o_msh_rd_rsp       (),    //[NUM_MSH_ROW_PORTS-1:0],   
/* msh_data_t       */ .o_msh_rd_data      (),    //[NUM_MSH_ROW_PORTS-1:0],
        
    ////////////////////
    // MSH writes
    ////////////////////
    
/* msh_row_crdts_t  */ .i_msh_wr_crdt_rtn  (),   //[NUM_MSH_PLANES-1:0],
   
/* msh_row_wr_req_t */ .o_msh_wr_req       (),   //[NUM_MSH_PLANES-1:0],
/* msh_data_t       */ .o_msh_wr_data      (),   //[NUM_MSH_PLANES-1:0],
  
    ////////////////////
    // MSH rd request
    ////////////////////
    
/* msh_row_crdts_t  */ .i_msh_rd_req_crdt_rtn (), //[NUM_MSH_PLANES-1:0],    

/* msh_row_rd_req_t */ .o_msh_rd_req          (), //[NUM_MSH_PLANES-1:0],
          
    ////////////////////
    // MSH rd response
    ////////////////////
    
/* msh_row_rd_rsp_t */ .i_msh_rd_rsp          (rrsp), //[NUM_MSH_PLANES-1:0],
/* msh_data_t       */ .i_msh_rd_data         (rddata), //[NUM_MSH_PLANES-1:0],

/* logic            */ .o_msh_rd_rsp_crdt_rtn (), //[NUM_MSH_PLANES-1:0],

    ////////////////////
    // MSH stubs 
    ////////////////////

/* msh_col_t        */ .o_eb_node_col         (),
/* msh_row_crdts_t  */ .o_wr_crdt_rtn         (), //[NUM_MSH_PLANES-1:0],
/* msh_row_crdts_t  */ .o_rd_req_crdt_rtn     (), //[NUM_MSH_PLANES-1:0],    
/* msh_row_rd_rsp_t */ .o_rd_rsp              (), //[NUM_MSH_PLANES-1:0],
/* msh_data_t       */ .o_rd_data             ()  //[NUM_MSH_PLANES-1:0]

);


endmodule 
