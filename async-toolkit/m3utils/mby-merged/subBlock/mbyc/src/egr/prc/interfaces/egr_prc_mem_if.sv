interface egr_prc_mem_if  ();
    
localparam RREQ_FIFO_W_DATA = 64;
localparam RREQ_FIFO_W_DEEP = 8;
localparam RREQ_FIFO_W_INST = 4;

// Signals for rrequ_fifo
logic [$clog2(RREQ_FIFO_W_DEEP)-1:0] rreq_fifo_rd_adr    [RREQ_FIFO_W_INST-1:0] ; 
logic                                rreq_fifo_rd_en     [RREQ_FIFO_W_INST-1:0] ; 
logic [$clog2(RREQ_FIFO_W_DEEP)-1:0] rreq_fifo_wr_adr    [RREQ_FIFO_W_INST-1:0] ; 
logic         [RREQ_FIFO_W_DATA-1:0] rreq_fifo_wr_data   [RREQ_FIFO_W_INST-1:0] ; 
logic                                rreq_fifo_wr_en     [RREQ_FIFO_W_INST-1:0] ; 
logic         [RREQ_FIFO_W_DATA-1:0] rreq_fifo_rd_data   [RREQ_FIFO_W_INST-1:0] ; 
logic                                rreq_fifo_rd_valid  [RREQ_FIFO_W_INST-1:0] ;

// memory modport
modport mem (
             // rreq
             input   rreq_fifo_rd_adr  ,   
             input   rreq_fifo_rd_en   ,
             input   rreq_fifo_wr_adr  ,   
             input   rreq_fifo_wr_data ,
             input   rreq_fifo_wr_en   ,
             output  rreq_fifo_rd_data ,
             output  rreq_fifo_rd_valid
            );
// client modport
modport cln (
             // rreq
             output  rreq_fifo_rd_adr  ,   
             output  rreq_fifo_rd_en   ,
             output  rreq_fifo_wr_adr  ,   
             output  rreq_fifo_wr_data ,
             output  rreq_fifo_wr_en   ,
             input   rreq_fifo_rd_data ,
             input   rreq_fifo_rd_valid
            );
    
endinterface:egr_prc_mem_if
