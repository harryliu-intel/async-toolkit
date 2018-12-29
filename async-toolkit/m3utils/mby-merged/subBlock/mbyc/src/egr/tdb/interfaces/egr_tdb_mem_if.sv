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
