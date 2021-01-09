`ifndef RTLGEN_INC_V3
`define RTLGEN_INC_V3

//lintra push -68094

// ===================================================
// Flop macros -- from Nebulon 2.01 - Bhuvan Meka - 071912  

`ifndef RTLGEN_FF
`define RTLGEN_FF(rtl_clk, rst_n, rst_val, d, q) \
    always_ff @(posedge rtl_clk, negedge rst_n) \
        if (!rst_n) q <= rst_val; \
        else        q <= d;
`endif // RTLGEN_FF

`ifndef RTLGEN_EN_FF
`define RTLGEN_EN_FF(rtl_clk, rst_n, rst_val, en, d, q) \
    always_ff @(posedge rtl_clk, negedge rst_n) \
        if (!rst_n) q <= rst_val; \
        else \
            if (en) q <= d;
`endif // RTLGEN_EN_FF

`ifndef RTLGEN_FF_SYNCRST
`define RTLGEN_FF_SYNCRST(rtl_clk, syncrst_n, rst_val, d, q) \
    always_ff @(posedge rtl_clk) \
        if (!syncrst_n) q <= rst_val; \
        else        q <= d;
`endif // RTLGEN_FF_SYNCRST

`ifndef RTLGEN_EN_FF_SYNCRST
`define RTLGEN_EN_FF_SYNCRST(rtl_clk, syncrst_n, rst_val, en, d, q) \
    always_ff @(posedge rtl_clk) \
        if (!syncrst_n) q <= rst_val; \
        else \
            if (en) q <= d;
`endif // RTLGEN_EN_FF_SYNCRST

`ifndef RTLGEN_FF_BOTHRST
`define RTLGEN_FF_BOTHRST(rtl_clk, rst_n, syncrst_n, rst_val, d, q) \
    always_ff @(posedge rtl_clk, negedge rst_n) \
        if (!rst_n) q <= rst_val; \
        else if (!syncrst_n) q <= rst_val; \
        else        q <= d;
`endif // RTLGEN_FF_BOTHRST

`ifndef RTLGEN_EN_FF_BOTHRST
`define RTLGEN_EN_FF_BOTHRST(rtl_clk, rst_n, syncrst_n, rst_val, en, d, q) \
    always_ff @(posedge rtl_clk, negedge rst_n) \
        if (!rst_n) q <= rst_val; \
        else if (!syncrst_n) q <= rst_val; \
        else if (en) q <= d;

`endif // RTLGEN_EN_FF_BOTHRST

// ===================================================================
// Latch macros -- compatible with nhm_macros RST_LATCH & EN_RST_LATCH

`ifndef RTLGEN_LATCH
`define RTLGEN_LATCH(rtl_clk, rst_n, rst_val, d, q) \
   always_latch                                     \
      begin                                         \
         if (rtl_clk)                               \
            if (!rst_n) q <= rst_val;               \
            else        q <=  d;                    \
      end                                           
`endif // RTLGEN_LATCH

`ifndef RTLGEN_EN_LATCH
`define RTLGEN_EN_LATCH(rtl_clk, rst_n, rst_val, en, d, q) \
   always_latch                                            \
      begin                                                \
         if (rtl_clk)                                      \
            if (!rst_n)     q <= rst_val;                  \
            else  if (en)   q <=  d;                       \
      end                                                  
`endif // RTLGEN_EN_LATCH

// ===================================================
// Latch macros -- from Nebulon 2.07p3 - Bhuvan Meka
//`ifndef RTLGEN_EN_LATCH
//`define RTLGEN_EN_LATCH(rst_n, rst_val, en, d, q) \
//    always_latch \
//        if (!rst_n) q <= rst_val; \
//        else \
//            if (en) q <= d;
//`endif // RTLGEN_EN_LATCH

// ===================================================
// Sidebande IOSF and Nebulon req, ack conversions 

`ifndef RTLGEN_CR_REQ_FROM_SB_REQ
`define RTLGEN_CR_REQ_FROM_SB_REQ(cr_req,sb_req) \
   assign cr_req.valid = sb_req.treg_irdy; \
   assign cr_req.opcode = sb_req.treg_opcode[3:0]; \
   assign cr_req.bar = sb_req.treg_bar; \
   assign cr_req.be = sb_req.treg_be;  \
   assign cr_req.fid = sb_req.treg_fid;  \
   assign cr_req.addr = sb_req.treg_addrlen ? (48'b0 | sb_req.treg_addr) :  (48'b0 | sb_req.treg_addr[15:0]); \
   assign cr_req.data = sb_req.treg_wdata; \
   assign cr_req.sai[7:0] = (sb_req.treg_ext_header[6:0] != 7'h0) || !sb_req.treg_eh || sb_req.treg_en_discard  ? 8'h0 :  sb_req.treg_ext_header[15:8];
`endif // RTLGEN_CR_REQ_FROM_SB_REQ

`ifndef RTLGEN_SB_ACK_FROM_CR_ACK
`define RTLGEN_SB_ACK_FROM_CR_ACK(sb_ack,cr_ack) \
   assign sb_ack.treg_trdy = (cr_ack.read_valid | cr_ack.write_valid); \
   assign sb_ack.treg_cerr = (cr_ack.read_miss | cr_ack.write_miss | ~cr_ack.sai_successfull); \
   assign sb_ack.treg_rdata = cr_ack.data; 
`endif // RTLGEN_SB_ACK_FROM_CR_ACK 

//lintra pop

`endif
