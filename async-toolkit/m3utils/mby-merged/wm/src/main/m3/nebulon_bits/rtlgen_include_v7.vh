`ifndef RTLGEN_INC_V7
`define RTLGEN_INC_V7

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
         if (!rst_n) q <= rst_val;                  \
         else if (rtl_clk) q <= d;                  \
      end                                           
`endif // RTLGEN_LATCH

`ifndef RTLGEN_EN_LATCH
`define RTLGEN_EN_LATCH(rtl_clk, rst_n, rst_val, en, d, q) \
   always_latch                                            \
      begin                                                \
         if (!rst_n) q <= rst_val;                         \
         else if (rtl_clk & en) q <= d;                    \
      end                                                  
`endif // RTLGEN_EN_LATCH

`ifndef RTLGEN_LATCH_SYNCRST
`define RTLGEN_LATCH_SYNCRST(rtl_clk, syncrst_n, rst_val, d, q) \
   always_latch                                     \
      begin                                         \
         if (rtl_clk)                               \
            if (!syncrst_n) q <= rst_val;           \
            else            q <=  d;                \
      end                                           
`endif // RTLGEN_LATCH_SYNCRST

`ifndef RTLGEN_EN_LATCH_SYNCRST
`define RTLGEN_EN_LATCH_SYNCRST(rtl_clk, syncrst_n, rst_val, en, d, q) \
   always_latch                                            \
      begin                                                \
         if (rtl_clk)                                      \
            if (!syncrst_n) q <= rst_val;                  \
            else if (en)    q <=  d;                       \
      end                                                  
`endif // RTLGEN_EN_LATCH_SYNCRST

`ifndef RTLGEN_FF_BOTHRST
`define RTLGEN_FF_BOTHRST(rtl_clk, rst_n, syncrst_n, rst_val, d, q) \
   always_latch                                     \
      begin                                         \
         if (!rst_n) q <= rst_val;                  \
         else if (rtl_clk)                          \
            if (!syncrst_n) q <= rst_val;           \
            else            q <=  d;                \
      end                                           
`endif // RTLGEN_FF_BOTHRST

`ifndef RTLGEN_EN_FF_BOTHRST
`define RTLGEN_EN_FF_BOTHRST(rtl_clk, rst_n, syncrst_n, rst_val, en, d, q) \
   always_latch                                     \
      begin                                         \
         if (!rst_n) q <= rst_val;                  \
         else if (rtl_clk)                          \
            if (!syncrst_n) q <= rst_val;           \
            else if (en)    q <=  d;                \
      end                                           
`endif // RTLGEN_EN_FF_BOTHRST


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

// sai_bits has two versions:
// old: treg_ext_header[15:8]
// new: treg_sai 
`ifndef RTLGEN_CR_REQ_FROM_SB
`define RTLGEN_CR_REQ_FROM_SB(cr_req,sb_prf,sai_data) \
   assign cr_req.valid = ``sb_prf``treg_irdy; \
   assign cr_req.opcode = cfg_opcode_t'(``sb_prf``treg_opcode[3:0]); \
   assign cr_req.bar = ``sb_prf``treg_bar; \
   assign cr_req.be = ``sb_prf``treg_be;  \
   assign cr_req.fid = ``sb_prf``treg_fid;  \
   assign cr_req.addr = ``sb_prf``treg_addrlen ? (48'b0 | ``sb_prf``treg_addr) :  (48'b0 | ``sb_prf``treg_addr[15:0]); \
   assign cr_req.data = ``sb_prf``treg_wdata; \
   assign cr_req.sai[7:0] = (``sb_prf``treg_ext_header[6:0] != 7'h0) || !``sb_prf``treg_eh || ``sb_prf``treg_eh_discard  ? 8'h0 :  ``sb_prf````sai_data``; 
`endif // RTLGEN_CR_REQ_FROM_SB

`ifndef RTLGEN_CR_REQ_FROM_SB_SIGNALS
`define RTLGEN_CR_REQ_FROM_SB_SIGNALS(cr_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,,treg_sai) 
`endif // RTLGEN_CR_REQ_FROM_SB_SIGNALS

`ifndef RTLGEN_CR_REQ_FROM_SB_REQ
`define RTLGEN_CR_REQ_FROM_SB_REQ(cr_req,sb_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,`` sb_req ``.,treg_sai) 
`endif // RTLGEN_CR_REQ_FROM_SB

`ifndef RTLGEN_CR_REQ_FROM_SB_SIGNALS_OLD
`define RTLGEN_CR_REQ_FROM_SB_SIGNALS_OLD(cr_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,,treg_ext_header[15:8]) 
`endif // RTLGEN_CR_REQ_FROM_SB_SIGNALS

`ifndef RTLGEN_CR_REQ_FROM_SB_REQ_OLD
`define RTLGEN_CR_REQ_FROM_SB_REQ_OLD(cr_req,sb_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,`` sb_req ``.,treg_ext_header[15:8]) 
`endif // RTLGEN_CR_REQ_FROM_SB


`ifndef RTLGEN_SB_FROM_CR_ACK
`define RTLGEN_SB_FROM_CR_ACK(cr_ack,sb_prf) \
   assign ``sb_prf``treg_trdy = (cr_ack.read_valid | cr_ack.write_valid); \
   assign ``sb_prf``treg_cerr = (cr_ack.read_miss | cr_ack.write_miss | ~cr_ack.sai_successfull); \
   assign ``sb_prf``treg_rdata = cr_ack.data;
`endif // RTLGEN_SB_FROM_CR_ACK

`ifndef RTLGEN_SB_ACK_FROM_CR_ACK
`define RTLGEN_SB_ACK_FROM_CR_ACK(sb_ack,cr_ack) \
   `RTLGEN_SB_FROM_CR_ACK(cr_ack,``sb_ack``.) 
`endif // RTLGEN_SB_ACK_FROM_CR_ACK

`ifndef RTLGEN_SB_SIGNALS_FROM_CR_ACK
`define RTLGEN_SB_SIGNALS_FROM_CR_ACK(cr_ack) \
   `RTLGEN_SB_FROM_CR_ACK(cr_ack,) 
`endif // RTLGEN_SB_SIGNALS_FROM_CR_ACK


// ===================================================
// Misc Macros 

`ifndef RTLGEN_LCB
`define RTLGEN_LCB(clock,enable,gate_clk) \
always_comb gate_clk = clock & enable;
`endif // RTLGEN_LCB



//lintra pop

`endif
