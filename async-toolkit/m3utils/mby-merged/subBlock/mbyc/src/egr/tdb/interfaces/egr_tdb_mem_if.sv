interface egr_tdb_mem_if  ();
    
localparam RREQ_BUF_W_DATA = 56;
localparam RREQ_BUF_W_DEEP = 8;
localparam RREQ_BUF_W_INST = 1;

localparam RRSP_BUF_W_DATA = 512;
localparam RRSP_BUF_W_DEEP = 32;
localparam RRSP_BUF_W_INST = 3;

// Signals for rrequ_buf
logic [$clog2(RREQ_BUF_W_DEEP)-1:0] rreq_buf_rd_adr    [RREQ_BUF_W_INST-1:0] ; 
logic                               rreq_buf_rd_en     [RREQ_BUF_W_INST-1:0] ; 
logic [$clog2(RREQ_BUF_W_DEEP)-1:0] rreq_buf_wr_adr    [RREQ_BUF_W_INST-1:0] ; 
logic         [RREQ_BUF_W_DATA-1:0] rreq_buf_wr_data   [RREQ_BUF_W_INST-1:0] ; 
logic                               rreq_buf_wr_en     [RREQ_BUF_W_INST-1:0] ; 
logic         [RREQ_BUF_W_DATA-1:0] rreq_buf_rd_data   [RREQ_BUF_W_INST-1:0] ; 
logic                               rreq_buf_rd_valid  [RREQ_BUF_W_INST-1:0] ;


// Signals for rrspu_buf
logic [$clog2(RRSP_BUF_W_DEEP)-1:0] rrsp_buf_rd_adr    [RRSP_BUF_W_INST-1:0] ; 
logic                               rrsp_buf_rd_en     [RRSP_BUF_W_INST-1:0] ; 
logic [$clog2(RRSP_BUF_W_DEEP)-1:0] rrsp_buf_wr_adr    [RRSP_BUF_W_INST-1:0] ; 
logic         [RRSP_BUF_W_DATA-1:0] rrsp_buf_wr_data   [RRSP_BUF_W_INST-1:0] ; 
logic                               rrsp_buf_wr_en     [RRSP_BUF_W_INST-1:0] ; 
logic         [RRSP_BUF_W_DATA-1:0] rrsp_buf_rd_data   [RRSP_BUF_W_INST-1:0] ; 
logic                               rrsp_buf_rd_valid  [RRSP_BUF_W_INST-1:0] ;


// memory modport
modport mem (
             // rreq
             input   rreq_buf_rd_adr  ,   
             input   rreq_buf_rd_en   ,
             input   rreq_buf_wr_adr  ,   
             input   rreq_buf_wr_data ,
             input   rreq_buf_wr_en   ,
             output  rreq_buf_rd_data ,
             output  rreq_buf_rd_valid,
             // rrsp
             input   rrsp_buf_rd_adr  ,   
             input   rrsp_buf_rd_en   ,
             input   rrsp_buf_wr_adr  ,   
             input   rrsp_buf_wr_data ,
             input   rrsp_buf_wr_en   ,
             output  rrsp_buf_rd_data ,
             output  rrsp_buf_rd_valid
            );
// client modport
modport cln (
             // rreq
             output  rreq_buf_rd_adr  ,   
             output  rreq_buf_rd_en   ,
             output  rreq_buf_wr_adr  ,   
             output  rreq_buf_wr_data ,
             output  rreq_buf_wr_en   ,
             input   rreq_buf_rd_data ,
             input   rreq_buf_rd_valid,
             // rrsp
             output  rrsp_buf_rd_adr  ,   
             output  rrsp_buf_rd_en   ,
             output  rrsp_buf_wr_adr  ,   
             output  rrsp_buf_wr_data ,
             output  rrsp_buf_wr_en   ,
             input   rrsp_buf_rd_data ,
             input   rrsp_buf_rd_valid
            );
    
endinterface:egr_tdb_mem_if


/*interface egr_tdb_mem_if ();

localparam RREQ_BUF_W_DATA = 56;
localparam RREQ_BUF_W_DEEP = 8;
localparam RREQ_BUF_W_INST = 1;

localparam RRSP_BUF_W_DATA = 512;
localparam RRSP_BUF_W_DEEP = 32;
localparam RRSP_BUF_W_INST = 3;


mby_mem_1r1w_if    #(.W_DATA(56),    .W_DEEP(8),   .W_INST(1) )    rreq_buf_if();
mby_mem_1r1w_if    #(.W_DATA(512),    .W_DEEP(32),   .W_INST(3) )  rrsp_buf_if();

modport mem(
    rreq_buf_if.mem,
    rrsp_buf_if.mem
);

modport cln(
    rreq_buf_if.cln,
    rrsp_buf_if.cln
);

endinterface:egr_tdb_mem_if*/
