interface egr_pfs_mem_epl_if  ();
    
localparam W_DATA = 32;
localparam W_DEEP = 1024;

// Signals for rrequ_fifo
logic [$clog2(W_DEEP)-1:0] rd_adr   ; 
logic                      rd_en    ; 
logic [$clog2(W_DEEP)-1:0] wr_adr   ; 
logic         [W_DATA-1:0] wr_data  ; 
logic                      wr_en    ; 
logic         [W_DATA-1:0] rd_data  ; 
logic                      rd_valid ;

// memory modport
modport mem (
             input   rd_adr  ,   
             input   rd_en   ,
             input   wr_adr  ,   
             input   wr_data ,
             input   wr_en   ,
             output  rd_data ,
             output  rd_valid
            );
// client modport
modport cln (
             output  rd_adr  ,   
             output  rd_en   ,
             output  wr_adr  ,   
             output  wr_data ,
             output  wr_en   ,
             input   rd_data ,
             input   rd_valid
            );
    
endinterface: egr_pfs_mem_epl_if