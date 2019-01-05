interface egr_mri_mem_if  ();
    
localparam RREQ_FIFO_W_DATA = 42;
localparam RREQ_FIFO_W_DEEP = 4;
localparam RREQ_FIFO_W_INST = 8;

localparam RRSP_FIFO_W_DATA = 528;
localparam RRSP_FIFO_W_DEEP = 4;
localparam RRSP_FIFO_W_INST = 8;

// Signals for rrequ_fifo
logic [$clog2(RREQ_FIFO_W_DEEP)-1:0] rreq_fifo_rd_adr    [RREQ_FIFO_W_INST-1:0] ; 
logic                                rreq_fifo_rd_en     [RREQ_FIFO_W_INST-1:0] ; 
logic [$clog2(RREQ_FIFO_W_DEEP)-1:0] rreq_fifo_wr_adr    [RREQ_FIFO_W_INST-1:0] ; 
logic         [RREQ_FIFO_W_DATA-1:0] rreq_fifo_wr_data   [RREQ_FIFO_W_INST-1:0] ; 
logic                                rreq_fifo_wr_en     [RREQ_FIFO_W_INST-1:0] ; 
logic         [RREQ_FIFO_W_DATA-1:0] rreq_fifo_rd_data   [RREQ_FIFO_W_INST-1:0] ; 
logic                                rreq_fifo_rd_valid  [RREQ_FIFO_W_INST-1:0] ;


// Signals for rrspu_fifo
logic [$clog2(RRSP_FIFO_W_DEEP)-1:0] rrsp_fifo_rd_adr    [RRSP_FIFO_W_INST-1:0] ; 
logic                                rrsp_fifo_rd_en     [RRSP_FIFO_W_INST-1:0] ; 
logic [$clog2(RRSP_FIFO_W_DEEP)-1:0] rrsp_fifo_wr_adr    [RRSP_FIFO_W_INST-1:0] ; 
logic         [RRSP_FIFO_W_DATA-1:0] rrsp_fifo_wr_data   [RRSP_FIFO_W_INST-1:0] ; 
logic                                rrsp_fifo_wr_en     [RRSP_FIFO_W_INST-1:0] ; 
logic         [RRSP_FIFO_W_DATA-1:0] rrsp_fifo_rd_data   [RRSP_FIFO_W_INST-1:0] ; 
logic                                rrsp_fifo_rd_valid  [RRSP_FIFO_W_INST-1:0] ;


// memory modport
modport mem (
             // rreq
             input   rreq_fifo_rd_adr  ,   
             input   rreq_fifo_rd_en   ,
             input   rreq_fifo_wr_adr  ,   
             input   rreq_fifo_wr_data ,
             input   rreq_fifo_wr_en   ,
             output  rreq_fifo_rd_data ,
             output  rreq_fifo_rd_valid,
             // rrsp
             input   rrsp_fifo_rd_adr  ,   
             input   rrsp_fifo_rd_en   ,
             input   rrsp_fifo_wr_adr  ,   
             input   rrsp_fifo_wr_data ,
             input   rrsp_fifo_wr_en   ,
             output  rrsp_fifo_rd_data ,
             output  rrsp_fifo_rd_valid
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
             input   rreq_fifo_rd_valid,
             // rrsp
             output  rrsp_fifo_rd_adr  ,   
             output  rrsp_fifo_rd_en   ,
             output  rrsp_fifo_wr_adr  ,   
             output  rrsp_fifo_wr_data ,
             output  rrsp_fifo_wr_en   ,
             input   rrsp_fifo_rd_data ,
             input   rrsp_fifo_rd_valid
            );
    
endinterface:egr_mri_mem_if
