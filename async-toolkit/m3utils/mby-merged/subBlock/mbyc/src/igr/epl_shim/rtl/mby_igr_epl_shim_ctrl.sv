//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
// -- Author       : Edward C. Ross
// -- Project Name : Madison Bay
// -- Description  : This block is the epl shim write control.  
//                   It interfaces with EPL, Packet Buffer(PB).
// -- Assumptions  : If a flit is an Eop then next valid flit is a Sop.
//------------------------------------------------------------------------------



module mby_igr_epl_shim_ctrl 
  import mby_igr_pkg::*, mby_igr_pb_pkg::*;
(

  input logic cclk,
  input logic rst,
  
// EPL I/O from MBY FS Dataplane Interface signals.
  input logic        i_port_e,  
  input dpc_ts_md_t    i_tsmd,     //from s1 ff
  
  output shim_md_t     o_seg0_md,
  output shim_md_t     o_seg1_md,
  output shim_md_t     o_seg2_md,
  output epl_ts_t     o_seg0_ts,
  output epl_ts_t     o_seg1_ts,
  output epl_ts_t     o_seg2_ts,
  output logic        o_seg0_sop_e,
  output logic        o_seg1_sop_e,
  output logic        o_seg2_sop_e,
  
  output shimfsel_t   o_seg0_sel,
  output shimfsel_t   o_seg1_sel,
  output shimfsel_t   o_seg2_sel,
  output logic [7:0]  o_seg0_we,
  output logic [7:0]  o_seg1_we,
  output logic [7:0]  o_seg2_we,
  output logic [2:0]  o_seg_e // enable aligned segment for write to PB
 
  
);

  logic  [4:0] current_flit; //24 8B flits for holding aligned data,
  logic  [4:0] nxt_flit; //update the current_flit position
  shim_md_t    s2_md;  //2 segments may be written simutaneously with different metadata.
  shim_md_t    qs2_md;  //2 segments may be written simutaneously with different metadata.
  epl_md_t     qs2_ts;  //2 segments may be written simutaneously with different metadata.
  shim_md_t    seg0_md;  //2 segments may be written simutaneously with different metadata.
  shim_md_t    seg1_md;
  shim_md_t    seg2_md;
  epl_ts_t     seg0_ts;
  epl_ts_t     seg1_ts;
  epl_ts_t     seg2_ts;

  logic  [7:0] seg0_we; //write enable for segment0 flits[7:0]
  logic  [7:0] seg1_we; //write enable for segment0 flits[15:8]
  logic  [7:0] seg2_we; //write enable for segment0 flits[23:16]
  logic  [2:0] seg_e;
  logic [31:0] fsel0;//EPL flit data select aligned data segment 0, each 4 bits is a sel field 4'b1000 is pad
  logic [31:0] fsel1;//EPL flit data select aligned data segment 0, each 4 bits is a sel field 4'b1000 is pad
  logic [31:0] fsel2;//EPL flit data select aligned data segment 0, each 4 bits is a sel field 4'b1000 is pad
  
  logic          sop_eop;
  logic      qs2_sop_eop;
  logic          eop_sop;
  logic      qs2_eop_sop;
  logic             rx_v;
  logic         qs2_rx_v;
  logic            sop_v;
  logic        qs2_sop_v;
  logic         flit_err; // the current flit count is incorrect set this bit.
  
//data, md may have sop that is written to a segemnt but segment is not written to PB.
//need to store sop state and write when segment is written to PB, the md for this flit will not have Sop
// set for the current segment.
//FIXME are there other md fields that need to be held until segment write and how to split along
// segment boundaries????
//FIXME WHAT ABOUT fcs_hint????
  logic      seg0_sop_e; //SOP in md for segment but segment not written to PB
  logic      seg1_sop_e; //SOP in md for segment but segment not written to PB,
  logic      seg2_sop_e; //SOP in md for segment but segment not written to PB,
  
  logic [7:0]      sflit;  //sop flit
  logic [7:0]  qs2_sflit;  //sop flit
  logic [7:0]      eflit;  //eop flit
  logic [7:0]  qs2_eflit;  //eop flit
  
  logic [3:0]    cnt_ones;
  logic [3:0] qs2_cnt_ones;

  shimfsel_t   s3q_seg0_sel;
  shimfsel_t   s3q_seg1_sel;
  shimfsel_t   s3q_seg2_sel;
  logic [7:0]  s3q_seg0_we;
  logic [7:0]  s3q_seg1_we;
  logic [7:0]  s3q_seg2_we;
  logic [2:0]  s3q_seg_e; // enable aligned segment for write to PB
  shim_md_t     s3q_seg0_md;
  shim_md_t     s3q_seg1_md;
  shim_md_t     s3q_seg2_md;
  epl_ts_t     s3q_seg0_ts;
  epl_ts_t     s3q_seg1_ts;
  epl_ts_t     s3q_seg2_ts; 
  logic        s3q_seg0_sop_e;  
  logic        s3q_seg1_sop_e;  
  logic        s3q_seg2_sop_e;  
    
//outputs begin
  assign o_seg0_sel = s3q_seg0_sel;
  assign o_seg1_sel = s3q_seg1_sel;
  assign o_seg2_sel = s3q_seg2_sel;
  assign o_seg0_we  = s3q_seg0_we;
  assign o_seg1_we  = s3q_seg1_we;
  assign o_seg2_we  = s3q_seg2_we;
  assign o_seg_e    = s3q_seg_e;
  assign o_seg0_md  = s3q_seg0_md;
  assign o_seg1_md  = s3q_seg1_md;
  assign o_seg2_md  = s3q_seg2_md;
  assign o_seg0_ts  = s3q_seg0_ts;
  assign o_seg1_ts  = s3q_seg1_ts;
  assign o_seg2_ts  = s3q_seg2_ts;
  assign o_seg0_sop_e = s3q_seg0_sop_e; 
  assign o_seg1_sop_e = s3q_seg1_sop_e; 
  assign o_seg2_sop_e = s3q_seg2_sop_e; 

  always_ff @(posedge cclk) s3q_seg0_sel <= fsel0;
  always_ff @(posedge cclk) s3q_seg1_sel <= fsel1;
  always_ff @(posedge cclk) s3q_seg2_sel <= fsel2;
  always_ff @(posedge cclk) s3q_seg0_we  <= seg0_we;
  always_ff @(posedge cclk) s3q_seg1_we  <= seg1_we;
  always_ff @(posedge cclk) s3q_seg2_we  <= seg2_we;
  always_ff @(posedge cclk) s3q_seg_e    <= seg_e;
  always_ff @(posedge cclk) s3q_seg0_md  <= seg0_md;
  always_ff @(posedge cclk) s3q_seg1_md  <= seg1_md;  
  always_ff @(posedge cclk) s3q_seg2_md  <= seg2_md;
  always_ff @(posedge cclk) s3q_seg0_ts  <= seg0_ts;
  always_ff @(posedge cclk) s3q_seg1_ts  <= seg1_ts;  
  always_ff @(posedge cclk) s3q_seg2_ts  <= seg2_ts;
  always_ff @(posedge cclk) s3q_seg0_sop_e <= seg0_sop_e;
  always_ff @(posedge cclk) s3q_seg1_sop_e <= seg1_sop_e;
  always_ff @(posedge cclk) s3q_seg2_sop_e <= seg2_sop_e;  
//outputs end
  

  assign cnt_ones = i_tsmd.md.flit_cnt; //number of valid rx_data flits from rx_data_v 0 to 8 flits 
  
   always_ff @(posedge cclk) current_flit <= (rst)? '0: nxt_flit;
   
//FIXME assertion never (rx_md.sop & rx_md.eop) & (rx_md.sop_pos == rx_md.eop_pos)

  assign rx_v  = i_port_e;
  assign eflit = (i_tsmd.md.eop_pos == 3'h0)? 8'b0000_0001:
                 (i_tsmd.md.eop_pos == 3'h1)? 8'b0000_0010:
                 (i_tsmd.md.eop_pos == 3'h2)? 8'b0000_0100:
                 (i_tsmd.md.eop_pos == 3'h3)? 8'b0000_1000:
                 (i_tsmd.md.eop_pos == 3'h4)? 8'b0001_0000:
                 (i_tsmd.md.eop_pos == 3'h5)? 8'b0010_0000:
                 (i_tsmd.md.eop_pos == 3'h6)? 8'b0100_0000: 8'b1000_0000;
  assign sflit = (i_tsmd.md.sop_pos == 3'h0)? 8'b0000_0001:
                 (i_tsmd.md.sop_pos == 3'h1)? 8'b0000_0010:
                 (i_tsmd.md.sop_pos == 3'h2)? 8'b0000_0100:
                 (i_tsmd.md.sop_pos == 3'h3)? 8'b0000_1000:
                 (i_tsmd.md.sop_pos == 3'h4)? 8'b0001_0000:
                 (i_tsmd.md.sop_pos == 3'h5)? 8'b0010_0000:
                 (i_tsmd.md.sop_pos == 3'h6)? 8'b0100_0000: 8'b1000_0000;
  assign sop_eop = (i_tsmd.md.sop_pos < i_tsmd.md.eop_pos);
  assign eop_sop = (i_tsmd.md.sop_pos > i_tsmd.md.eop_pos);
  assign sop_v   = (rx_v) & i_tsmd.md.sop;

//s2
  assign s2_md = {i_tsmd.md.multi,
                  i_tsmd.md.fast,
                  i_tsmd.md.fcs_hint,
                  i_tsmd.md.dei,
                  i_tsmd.md.error,
                  i_tsmd.md.eop,
                  i_tsmd.md.eop_pos,
                  i_tsmd.md.byte_pos,
                  i_tsmd.md.sop,
                  i_tsmd.md.sop_pos,
                  i_tsmd.md.tc};
  always_ff @(posedge cclk)qs2_rx_v    <= rx_v;
  always_ff @(posedge cclk)qs2_cnt_ones <= cnt_ones;
  always_ff @(posedge cclk)qs2_eflit   <= eflit;
  always_ff @(posedge cclk)qs2_sflit   <= sflit;
  always_ff @(posedge cclk)qs2_sop_eop <= sop_eop;
  always_ff @(posedge cclk)qs2_eop_sop <= eop_sop;
  always_ff @(posedge cclk)qs2_sop_v   <= sop_v;
  
  always_ff @(posedge cclk) qs2_md <= (rst)? '0: (rx_v)? s2_md: qs2_md;
  always_ff @(posedge cclk) qs2_ts <= (rst)? '0: (rx_v)? i_tsmd.ts: qs2_ts; 
  always_comb
  begin
    seg0_md = qs2_md;
    seg1_md = qs2_md;
    seg2_md = qs2_md;
    seg0_ts = qs2_ts;
    seg1_ts = qs2_ts;
    seg2_ts = qs2_ts;
    seg0_md.sop_pos = 3'h0;  //out of shim sop always in flit[0]
    seg1_md.sop_pos = 3'h0;  //out of shim sop always in flit[0]
    seg2_md.sop_pos = 3'h0;  //out of shim sop always in flit[0]

    nxt_flit  = current_flit;
    seg0_sop_e = '0;
    seg1_sop_e = '0;
    seg2_sop_e = '0;
    seg0_we    = '0;
    seg1_we    = '0;       
    seg2_we    = '0;
    fsel0      = '0;
    fsel1      = '0;
    fsel2      = '0;
    seg_e      = '0;
    flit_err   = '0;
    if(qs2_rx_v) begin
      if(qs2_md.eop) begin //this case handles unaligned segments with valid EOP 
      unique case(current_flit) inside  //FIXME shoud current_flit be a 1-hot vector[23:0]??
        5'd0: begin
          seg0_we[7:0] = 8'b1111_1111;
          seg_e        = 3'b001;
          seg1_sop_e   = qs2_md.sop & (qs2_md.sop_pos != 3'b000);  //capture sop md for next segment
          seg0_md.sop  = qs2_md.sop & (qs2_md.sop_pos == 3'b000);
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0            = 32'h8888_8880;
              seg_e            = 3'b001;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0            = 32'h8888_8810; //FIXME anything to do with FCS_hints???
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0            = 32'h8888_8210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h8888_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0            = 32'h8884_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0            = 32'h8854_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0            = 32'h8654_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0            = 32'h7654_3210;
              nxt_flit         = 5'd08;
              flit_err =qs2_sop_v & (~qs2_sflit[0]); //If there is a valid Sop and Eop is in flit[7] it has to be in flit[0]
            end                        
          endcase //reverse
        end //d0
        5'd1: begin
          seg0_we[7:0] = 8'b1111_1110;
          seg_e         = 3'b001;
          seg1_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg0_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0           = 32'h8888_8800;
              seg0_md.eop_pos = 3'd1;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0           = 32'h8888_8100;
              seg0_md.eop_pos = 3'd2;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0           = 32'h8888_2100;
              seg0_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h8883_2100;
              seg0_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0            = 32'h8843_2100;
              seg0_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0            = 32'h8543_2100;
              seg0_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0            = 32'h6543_2100;
              seg0_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0            = 32'h6543_2100;
              fsel1            = 32'h8888_8887;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos  = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err         =qs2_sop_v; //There should not be a valid Sop 
            end                         
          endcase //reverse
        end //d1
        5'd2: begin
          seg0_we[7:0] = 8'b1111_1100;
          seg_e        = 3'b001;
          seg1_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg0_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0            = 32'h8888_8000;
              seg0_md.eop_pos = 3'd2;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0            = 32'h8888_1000;
              seg0_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0            = 32'h8882_1000;
              seg0_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h8832_1000;
              seg0_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0            = 32'h8432_1000;
              seg0_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0            = 32'h5432_1000;
              seg0_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0            = 32'h5432_1000;
              fsel1            = 32'h8888_8886;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd0;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop;end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0            = 32'h5432_1000;
              fsel1            = 32'h8888_8876;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd1;
              seg1_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse          
        end //d2
        5'd3: begin
          seg0_we[7:0] = 8'b1111_1000;
          seg_e        = 3'b001;
          seg1_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg0_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0            = 32'h8888_0000;
              seg0_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0            = 32'h8881_0000;
              seg0_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0            = 32'h8821_0000;
              seg0_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h8321_0000;
              seg0_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0            = 32'h4321_0000;
              seg0_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0            = 32'h4321_0000;
              fsel1            = 32'h8888_8885;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop;end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop;end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0            = 32'h4321_0000;
              fsel1            = 32'h8888_8865;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd1;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0           = 32'h4321_0000;
              fsel1           = 32'h8888_8765;
              seg0_md.eop     = 1'b0;
              seg1_md.eop_pos = 3'd2;
              seg1_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b011;
              nxt_flit        = 5'd16;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d3          
        5'd4: begin
          seg0_we[7:0] = 8'b1111_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg0_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0            = 32'h8880_0000;
              seg0_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0            = 32'h8810_0000;
              seg0_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0            = 32'h8210_0000;
              seg0_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h3210_0000;
              seg0_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_8884;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_8854;
              seg1_md.eop_pos = 3'd1;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_8654;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd2;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_7654;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd3;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d4
        5'd5: begin
          seg0_we[7:0] = 8'b1110_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg0_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0            = 32'h8800_0000;
              seg0_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0            = 32'h8100_0000;
              seg0_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0            = 32'h2100_0000;
              seg0_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h2100_0000;
              fsel1            = 32'h8888_8883;
              seg1_md.eop_pos = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0           = 32'h2100_0000;
              fsel1           = 32'h8888_8843;
              seg1_md.eop_pos = 3'd1;
              seg1_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0           = 32'h2100_0000;
              fsel1           = 32'h8888_8543;
              seg1_md.eop_pos = 3'd2;
              seg1_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0           = 32'h2100_0000;
              fsel1           = 32'h8888_6543;
              seg0_md.eop     = 1'b0;
              seg1_md.eop_pos = 3'd3;
              seg1_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0           = 32'h2100_0000;
              fsel1           = 32'h8887_6543;
              seg0_md.eop     = 1'b0;
              seg1_md.eop_pos = 3'd4;
              seg1_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b011;
              nxt_flit        = 5'd16;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d5
        5'd6: begin
          seg0_we[7:0] = 8'b1100_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg0_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0            = 32'h8000_0000;
              seg0_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0            = 32'h1000_0000;
              seg0_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_8882;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd0;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_8832;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd1;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_8432;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd2;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_5432;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd3;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8886_5432;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd4;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8876_5432;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd5;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d6
        5'd7: begin
          seg0_we[7:0] = 8'b1000_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg0_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel0            = 32'h0000_0000;
              seg0_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_8881;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd0;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_8821;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd1;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_8321;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd2;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_4321;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd3;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8885_4321;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd4;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8865_4321;
              seg0_md.eop      = 1'b0;
              seg1_md.eop_pos = 3'd5;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17;
                            seg1_sop_e = 1'b0; seg2_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8765_4321;
              seg1_md.eop_pos = 3'd6;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d7
//end of seg0
        5'd8: begin
          seg1_we[7:0] = 8'b1111_1111;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop & (qs2_md.sop_pos != 3'b000);  //capture sop md for next segment
          seg1_md.sop  = qs2_md.sop & (qs2_md.sop_pos == 3'b000);
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h8888_8880;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1            = 32'h8888_8810; //FIXME anything to do with FCS_hints???
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1            = 32'h8888_8210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1            = 32'h8888_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1            = 32'h8884_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1            = 32'h8854_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1            = 32'h8654_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h7654_3210;
              nxt_flit         = 5'd16;
              flit_err =qs2_sop_v & (~qs2_sflit[0]); //If there is a valid Sop and Eop is in flit[7] it has to be in flit[0]
            end                        
          endcase //reverse
        end //d8
        5'd9: begin
          seg1_we[7:0] = 8'b1111_1110;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg1_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h8888_8800;
              seg1_md.eop_pos = 3'd1;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1            = 32'h8888_8100;
              seg1_md.eop_pos = 3'd2;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1            = 32'h8888_2100;
              seg1_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1            = 32'h8883_2100;
              seg1_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1            = 32'h8843_2100;
              seg1_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1            = 32'h8543_2100;
              seg1_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1            = 32'h6543_2100;
              seg1_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h6543_2100;
              fsel2            = 32'h8888_8887;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd0;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err         =qs2_sop_v; //There should not be a valid Sop 
            end                         
          endcase //reverse
        end //d9
        5'd10: begin
          seg1_we[7:0] = 8'b1111_1100;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg1_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h8888_8000;
              seg1_md.eop_pos = 3'd2;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1            = 32'h8888_1000;
              seg1_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1            = 32'h8882_1000;
              seg1_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1            = 32'h8832_1000;
              seg1_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1            = 32'h8432_1000;
              seg1_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1            = 32'h5432_1000;
              seg1_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1           = 32'h5432_1000;
              fsel2           = 32'h8888_8886;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd0;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd1;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h5432_1000;
              fsel2            = 32'h8888_8876;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd1;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse          
        end //d10
        5'd11: begin
          seg1_we[7:0] = 8'b1111_1000;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg1_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h8888_0000;
              seg1_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1            = 32'h8881_0000;
              seg1_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1            = 32'h8821_0000;
              seg1_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1            = 32'h8321_0000;
              seg1_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1            = 32'h4321_0000;
              seg1_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1            = 32'h4321_0000;
              fsel2            = 32'h8888_8885;
              seg2_md.eop_pos = 3'd0;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1            = 32'h4321_0000;
              fsel2            = 32'h8888_8865;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd1;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h4321_0000;
              fsel2            = 32'h8888_8765;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd2;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d11          
        5'd12: begin
          seg1_we[7:0] = 8'b1111_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg1_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h8880_0000;
              seg1_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1            = 32'h8810_0000;
              seg1_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1            = 32'h8210_0000;
              seg1_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1            = 32'h3210_0000;
              seg1_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_8884;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd0;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_8854;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd1;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_8654;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd2;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_7654;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd3;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d12
        5'd13: begin
          seg1_we[7:0] = 8'b1110_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg1_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h8800_0000;
              seg1_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1            = 32'h8100_0000;
              seg1_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1            = 32'h2100_0000;
              seg1_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_8883;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd0;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_8843;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd1;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_8543;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd2;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_6543;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd3;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8887_6543;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd4;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              nxt_flit         = 5'd0;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d13
        5'd14: begin
          seg1_we[7:0] = 8'b1100_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg1_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h8000_0000;
              seg1_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1            = 32'h1000_0000;
              seg1_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1           = 32'h1000_0000;
              fsel2           = 32'h8888_8882;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd0;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8888_8832;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd1;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1           = 32'h1000_0000;
              fsel2           = 32'h8888_8432;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd2;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1           = 32'h1000_0000;
              fsel2           = 32'h8888_5432;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd3;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1           = 32'h1000_0000;
              fsel2           = 32'h8886_5432;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd4;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8876_5432;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd5;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              nxt_flit         = 5'd0;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d14
        5'd15: begin
          seg1_we[7:0] = 8'b1000_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg1_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel1            = 32'h0000_0000;
              seg1_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel1           = 32'h0000_0000;
              fsel2           = 32'h8888_8881;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd0;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel1           = 32'h0000_0000;
              fsel2           = 32'h8888_8821;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd1;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel1           = 32'h0000_0000;
              fsel2           = 32'h8888_8321;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd2;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel1           = 32'h0000_0000;
              fsel2           = 32'h8888_4321;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd3;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel1           = 32'h0000_0000;
              fsel2           = 32'h8885_4321;
              seg1_md.eop     = 1'b0;
              seg2_md.eop_pos = 3'd4;
              seg2_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8865_4321;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd5;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01;
                            seg2_sop_e = 1'b0; seg0_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8765_4321;
              seg1_md.eop      = 1'b0;
              seg2_md.eop_pos = 3'd6;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              nxt_flit         = 5'd0;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d15
//end of seg1
        5'd16: begin
          seg2_we[7:0] = 8'b1111_1111;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop & (qs2_md.sop_pos != 3'b000);  //capture sop md for next segment
          seg2_md.sop  = qs2_md.sop & (qs2_md.sop_pos == 3'b000);
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2            = 32'h8888_8880;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2            = 32'h8888_8810; //FIXME anything to do with FCS_hints???
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2            = 32'h8888_8210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2            = 32'h8888_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2            = 32'h8884_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2            = 32'h8854_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2            = 32'h8654_3210;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2            = 32'h7654_3210;
              nxt_flit         = 5'd0;
              flit_err =qs2_sop_v & (~qs2_sflit[0]); //If there is a valid Sop and Eop is in flit[7] it has to be in flit[0]
            end                        
          endcase //reverse
        end //d16
        5'd17: begin
          seg2_we[7:0] = 8'b1111_1110;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg2_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2            = 32'h8888_8800;
              seg2_md.eop_pos = 3'd1;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2            = 32'h8888_8100;
              seg2_md.eop_pos = 3'd2;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2            = 32'h8888_2100;
              seg2_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2            = 32'h8883_2100;
              seg2_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2            = 32'h8843_2100;
              seg2_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2            = 32'h8543_2100;
              seg2_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2            = 32'h6543_2100;
              seg2_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2            = 32'h6543_2100;
              fsel0            = 32'h8888_8887;
              seg2_md.eop      = 1'b0;
              seg0_md.eop_pos = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err         =qs2_sop_v; //There should not be a valid Sop 
            end                         
          endcase //reverse
        end //d17
        5'd18: begin
          seg2_we[7:0] = 8'b1111_1100;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg2_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2            = 32'h8888_8000;
              seg2_md.eop_pos = 3'd2;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2            = 32'h8888_1000;
              seg2_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2            = 32'h8882_1000;
              seg2_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2            = 32'h8832_1000;
              seg2_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2            = 32'h8432_1000;
              seg2_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2            = 32'h5432_1000;
              seg2_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2            = 32'h5432_1000;
              fsel0            = 32'h8888_8886;
              seg2_md.eop      = 1'b0;
              seg0_md.eop_pos = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2            = 32'h5432_1000;
              fsel0            = 32'h8888_8876;
              seg2_md.eop      = 1'b0;
              seg0_md.eop_pos = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse          
        end //d18
        5'd19: begin
          seg2_we[7:0] = 8'b1111_1000;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg2_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2            = 32'h8888_0000;
              seg2_md.eop_pos = 3'd3;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2            = 32'h8881_0000;
              seg2_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2            = 32'h8821_0000;
              seg2_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2            = 32'h8321_0000;
              seg2_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2            = 32'h4321_0000;
              seg2_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2           = 32'h4321_0000;
              fsel0           = 32'h8888_8885;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2            = 32'h4321_0000;
              fsel0            = 32'h8888_8865;
              seg2_md.eop      = 1'b0;
              seg0_md.eop_pos = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2           = 32'h4321_0000;
              fsel0           = 32'h8888_8765;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd2;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              nxt_flit        = 5'd08;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d19          
        5'd20: begin
          seg2_we[7:0] = 8'b1111_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg2_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2            = 32'h8880_0000;
              seg2_md.eop_pos = 3'd4;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2            = 32'h8810_0000;
              seg2_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2            = 32'h8210_0000;
              seg2_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2            = 32'h3210_0000;
              seg2_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2           = 32'h3210_0000;
              fsel0           = 32'h8888_8884;
              seg0_md.eop_pos = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2           = 32'h3210_0000;
              fsel0           = 32'h8888_8854;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2            = 32'h3210_0000;
              fsel0            = 32'h8888_8654;
              seg2_md.eop      = 1'b0;
              seg0_md.eop_pos = 3'd2;
              seg0_we[7:0]      = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2           = 32'h3210_0000;
              fsel0           = 32'h8888_7654;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd3;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              nxt_flit        = 5'd08;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d20
        5'd21: begin
          seg2_we[7:0] = 8'b1110_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg2_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2           = 32'h8800_0000;
              seg2_md.eop_pos = 3'd5;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2            = 32'h8100_0000;
              seg2_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2            = 32'h2100_0000;
              seg2_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2           = 32'h2100_0000;
              fsel0           = 32'h8888_8883;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2           = 32'h2100_0000;
              fsel0           = 32'h8888_8843;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2           = 32'h2100_0000;
              fsel0           = 32'h8888_8543;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd2;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2            = 32'h2100_0000;
              fsel0            = 32'h8888_6543;
              seg2_md.eop      = 1'b0;
              seg0_md.eop_pos = 3'd3;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2           = 32'h2100_0000;
              fsel0           = 32'h8887_6543;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd4;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              nxt_flit        = 5'd08;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d21
        5'd22: begin
          seg2_we[7:0] = 8'b1100_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg2_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2           = 32'h8000_0000;
              seg2_md.eop_pos = 3'd6;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2            = 32'h1000_0000;
              seg2_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2           = 32'h1000_0000;
              fsel0           = 32'h8888_8882;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2           = 32'h1000_0000;
              fsel0           = 32'h8888_8832;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2           = 32'h1000_0000;
              fsel0           = 32'h8888_8432;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd2;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2           = 32'h1000_0000;
              fsel0           = 32'h8888_5432;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd3;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2           = 32'h1000_0000;
              fsel0           = 32'h8886_5432;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd4;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2            = 32'h1000_0000;
              fsel0            = 32'h8876_5432;
              seg2_md.eop      = 1'b0;
              seg0_md.eop_pos = 3'd5;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d22
        5'd23: begin
          seg2_we[7:0] = 8'b1000_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = qs2_md.sop;  //capture sop md for next segment
          seg2_md.sop  = 1'b0; //this segment will not have a sop
          unique case(1'b1) inside
           qs2_eflit[0]: begin
              fsel2           = 32'h0000_0000;
              seg2_md.eop_pos = 3'd7;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & (qs2_sflit[0] | ((qs2_cnt_ones > 4'd1) & ~qs2_sflit[1]));
            end
           qs2_eflit[1]: begin
              fsel2           = 32'h0000_0000;
              fsel0           = 32'h8888_8881;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[1:0]) | ((qs2_cnt_ones > 4'd2) & ~qs2_sflit[2]));
            end
           qs2_eflit[2]: begin
              fsel2           = 32'h0000_0000;
              fsel0           = 32'h8888_8821;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[2:0]) | ((qs2_cnt_ones > 4'd3) & ~qs2_sflit[3]));
            end
           qs2_eflit[3]: begin
              fsel2           = 32'h0000_0000;
              fsel0           = 32'h8888_8321;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd2;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[3:0]) | ((qs2_cnt_ones > 4'd4) & ~qs2_sflit[4]));
            end            
           qs2_eflit[4]: begin
              fsel2           = 32'h0000_0000;
              fsel0           = 32'h8888_4321;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd3;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;              
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[4:0]) | ((qs2_cnt_ones > 4'd5) & ~qs2_sflit[5]));
            end            
           qs2_eflit[5]: begin
              fsel2           = 32'h0000_0000;
              fsel0           = 32'h8885_4321;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd4;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[5:0]) | ((qs2_cnt_ones > 4'd6) & ~qs2_sflit[6]));
            end            
           qs2_eflit[6]: begin
              fsel2           = 32'h0000_0000;
              fsel0           = 32'h8865_4321;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd5;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09;
                            seg0_sop_e = 1'b0; seg1_sop_e = qs2_md.sop; end  //In this case sop is put into seg2
                default: nxt_flit = 5'd08;
              endcase //qs2_cnt_ones
              flit_err =qs2_sop_v & ((|qs2_sflit[6:0]) | ((qs2_cnt_ones > 4'd7) & ~qs2_sflit[7]));
            end            
           qs2_eflit[7]: begin
              fsel2           = 32'h0000_0000;
              fsel0           = 32'h8765_4321;
              seg2_md.eop     = 1'b0;
              seg0_md.eop_pos = 3'd6;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e           = 3'b101;
              nxt_flit        = 5'd08;
              flit_err =qs2_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d23
//end of seg2
      endcase //current_flit
      end //if(eop)
      else begin  //no EOP so the unaligned flit is full sop should only then be in flits 0, 8, 16
        unique case(current_flit) inside  //FIXME shoud current_flit be a 1-hot vector[23:0]??
          5'd0: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b0000_0001; nxt_flit = 5'd01; end
              4'd2: begin fsel0 = 32'h0000_0010; seg0_we[7:0] = 8'b0000_0001; nxt_flit = 5'd02; end
              4'd3: begin fsel0 = 32'h0000_0210; seg0_we[7:0] = 8'b0000_0111; nxt_flit = 5'd03; end
              4'd4: begin fsel0 = 32'h0000_3210; seg0_we[7:0] = 8'b0000_1111; nxt_flit = 5'd04; end
              4'd5: begin fsel0 = 32'h0004_3210; seg0_we[7:0] = 8'b0001_1111; nxt_flit = 5'd05; end
              4'd6: begin fsel0 = 32'h0054_3210; seg0_we[7:0] = 8'b0011_1111; nxt_flit = 5'd06; end
              4'd7: begin fsel0 = 32'h0654_3210; seg0_we[7:0] = 8'b1111_1111; nxt_flit = 5'd07; end
              4'd8: begin fsel0 = 32'h7654_3210; seg0_we[7:0] = 8'b1111_1111; nxt_flit = 5'd08; seg_e = 3'b001; end
              default: nxt_flit = 5'd00;
            endcase //qs2_cnt_ones
          end //d0
          5'd1: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b0000_0010; nxt_flit = 5'd02; end
              4'd2: begin fsel0 = 32'h0000_0100; seg0_we[7:0] = 8'b0000_0110; nxt_flit = 5'd03; end
              4'd3: begin fsel0 = 32'h0000_2100; seg0_we[7:0] = 8'b0000_1110; nxt_flit = 5'd04; end
              4'd4: begin fsel0 = 32'h0003_2100; seg0_we[7:0] = 8'b0001_1110; nxt_flit = 5'd05; end
              4'd5: begin fsel0 = 32'h0043_2100; seg0_we[7:0] = 8'b0011_1110; nxt_flit = 5'd06; end
              4'd6: begin fsel0 = 32'h0543_2100; seg0_we[7:0] = 8'b0111_1110; nxt_flit = 5'd07; end
              4'd7: begin fsel0 = 32'h6543_2100; seg0_we[7:0] = 8'b1111_1110; nxt_flit = 5'd08; seg_e = 3'b001; end
              4'd8: begin fsel0 = 32'h6543_2100; seg0_we[7:0] = 8'b1111_1110; nxt_flit = 5'd09; seg_e = 3'b001;
                          fsel1 = 32'h0000_0007; seg1_we[7:0] = 8'b0000_0001; end
              default: nxt_flit = 5'd01;
            endcase //qs2_cnt_ones
          end //d0
          5'd2: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b0000_0100; nxt_flit = 5'd03; end
              4'd2: begin fsel0 = 32'h0000_1000; seg0_we[7:0] = 8'b0000_1100; nxt_flit = 5'd04; end
              4'd3: begin fsel0 = 32'h0002_1000; seg0_we[7:0] = 8'b0001_1100; nxt_flit = 5'd05; end
              4'd4: begin fsel0 = 32'h0032_1000; seg0_we[7:0] = 8'b0011_1100; nxt_flit = 5'd06; end
              4'd5: begin fsel0 = 32'h0432_1000; seg0_we[7:0] = 8'b0111_1100; nxt_flit = 5'd07; end
              4'd6: begin fsel0 = 32'h5432_1000; seg0_we[7:0] = 8'b1111_1100; nxt_flit = 5'd08; seg_e = 3'b001; end
              4'd7: begin fsel0 = 32'h5432_1000; seg0_we[7:0] = 8'b1111_1100; nxt_flit = 5'd09; seg_e = 3'b001;
                          fsel1 = 32'h0000_0006; seg1_we[7:0] = 8'b0000_0001; end
              4'd8: begin fsel0 = 32'h5432_1000; seg0_we[7:0] = 8'b1111_1100; nxt_flit = 5'd10; seg_e = 3'b001;
                          fsel1 = 32'h0000_0076; seg1_we[7:0] = 8'b0000_0011; end
              default: nxt_flit = 5'd02;
            endcase //qs2_cnt_ones
          end //d0
          5'd3: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b0000_1000; nxt_flit = 5'd04; end
              4'd2: begin fsel0 = 32'h0001_0000; seg0_we[7:0] = 8'b0001_1000; nxt_flit = 5'd05; end
              4'd3: begin fsel0 = 32'h0021_0000; seg0_we[7:0] = 8'b0011_1000; nxt_flit = 5'd06; end
              4'd4: begin fsel0 = 32'h0321_0000; seg0_we[7:0] = 8'b0111_1000; nxt_flit = 5'd07; end
              4'd5: begin fsel0 = 32'h4321_0000; seg0_we[7:0] = 8'b1111_1000; nxt_flit = 5'd08; seg_e = 3'b001; end
              4'd6: begin fsel0 = 32'h4321_0000; seg0_we[7:0] = 8'b1111_1000; nxt_flit = 5'd09; seg_e = 3'b001;
                          fsel1 = 32'h0000_0005; seg1_we[7:0] = 8'b0000_0001; end
              4'd7: begin fsel0 = 32'h4321_0000; seg0_we[7:0] = 8'b1111_1000; nxt_flit = 5'd10; seg_e = 3'b001;
                          fsel1 = 32'h0000_0065; seg1_we[7:0] = 8'b0000_0011; end
              4'd8: begin fsel0 = 32'h4321_0000; seg0_we[7:0] = 8'b1111_1000; nxt_flit = 5'd11; seg_e = 3'b001;
                          fsel1 = 32'h0000_0765; seg1_we[7:0] = 8'b0000_0111; end
              default: nxt_flit = 5'd03;
            endcase //qs2_cnt_ones
          end //d0
          5'd4: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b0001_0000; nxt_flit = 5'd05; end
              4'd2: begin fsel0 = 32'h0010_0000; seg0_we[7:0] = 8'b0011_0000; nxt_flit = 5'd06; end
              4'd3: begin fsel0 = 32'h0210_0000; seg0_we[7:0] = 8'b0111_0000; nxt_flit = 5'd07; end
              4'd4: begin fsel0 = 32'h3210_0000; seg0_we[7:0] = 8'b1111_0000; nxt_flit = 5'd08; seg_e = 3'b001; end
              4'd5: begin fsel0 = 32'h3210_0000; seg0_we[7:0] = 8'b1111_0000; nxt_flit = 5'd09; seg_e = 3'b001;
                          fsel1 = 32'h0000_0004; seg1_we[7:0] = 8'b0000_0001;end
              4'd6: begin fsel0 = 32'h3210_0000; seg0_we[7:0] = 8'b1111_0000; nxt_flit = 5'd10; seg_e = 3'b001;
                          fsel1 = 32'h0000_0054; seg1_we[7:0] = 8'b0000_0011; end
              4'd7: begin fsel0 = 32'h3210_0000; seg0_we[7:0] = 8'b1111_0000; nxt_flit = 5'd11; seg_e = 3'b001;
                          fsel1 = 32'h0000_0654; seg1_we[7:0] = 8'b0000_0111; end
              4'd8: begin fsel0 = 32'h3210_0000; seg0_we[7:0] = 8'b1111_0000; nxt_flit = 5'd12; seg_e = 3'b001;
                          fsel1 = 32'h0000_7654; seg1_we[7:0] = 8'b0000_1111; end
              default: nxt_flit = 5'd04;
            endcase //qs2_cnt_ones
          end //d0
          5'd5: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b0010_0000; nxt_flit = 5'd06; end
              4'd2: begin fsel0 = 32'h0100_0000; seg0_we[7:0] = 8'b0110_0000; nxt_flit = 5'd07; end
              4'd3: begin fsel0 = 32'h2100_0000; seg0_we[7:0] = 8'b1110_0000; nxt_flit = 5'd08; seg_e = 3'b001; end
              4'd4: begin fsel0 = 32'h2100_0000; seg0_we[7:0] = 8'b1110_0000; nxt_flit = 5'd09; seg_e = 3'b001;
                          fsel1 = 32'h0000_0003; seg1_we[7:0] = 8'b0000_0001; end
              4'd5: begin fsel0 = 32'h2100_0000; seg0_we[7:0] = 8'b1110_0000; nxt_flit = 5'd10; seg_e = 3'b001;
                          fsel1 = 32'h0000_0043; seg1_we[7:0] = 8'b0000_0011; end
              4'd6: begin fsel0 = 32'h2100_0000; seg0_we[7:0] = 8'b1110_0000; nxt_flit = 5'd11; seg_e = 3'b001;
                          fsel1 = 32'h0000_0543; seg1_we[7:0] = 8'b0000_0111; end
              4'd7: begin fsel0 = 32'h2100_0000; seg0_we[7:0] = 8'b1110_0000; nxt_flit = 5'd12; seg_e = 3'b001;
                          fsel1 = 32'h0000_6543; seg1_we[7:0] = 8'b0000_1111; end
              4'd8: begin fsel0 = 32'h2100_0000; seg0_we[7:0] = 8'b1110_0000; nxt_flit = 5'd13; seg_e = 3'b001;
                          fsel1 = 32'h0007_6543; seg1_we[7:0] = 8'b0001_1111; end
              default: nxt_flit = 5'd05;
            endcase //qs2_cnt_ones
          end //d0 
          5'd6: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b0100_0000; nxt_flit = 5'd07; end
              4'd2: begin fsel0 = 32'h1000_0000; seg0_we[7:0] = 8'b1100_0000; nxt_flit = 5'd08; seg_e = 3'b001; end
              4'd3: begin fsel0 = 32'h1000_0000; seg0_we[7:0] = 8'b1100_0000; nxt_flit = 5'd09; seg_e = 3'b001;
                          fsel1 = 32'h0000_0002; seg1_we[7:0] = 8'b0000_0001; end
              4'd4: begin fsel0 = 32'h1000_0000; seg0_we[7:0] = 8'b1100_0000; nxt_flit = 5'd10; seg_e = 3'b001;
                          fsel1 = 32'h0000_0032; seg1_we[7:0] = 8'b0000_0011; end
              4'd5: begin fsel0 = 32'h1000_0000; seg0_we[7:0] = 8'b1100_0000; nxt_flit = 5'd11; seg_e = 3'b001;
                          fsel1 = 32'h0000_0432; seg1_we[7:0] = 8'b0000_0111; end
              4'd6: begin fsel0 = 32'h1000_0000; seg0_we[7:0] = 8'b1100_0000; nxt_flit = 5'd12; seg_e = 3'b001;
                          fsel1 = 32'h0000_5432; seg1_we[7:0] = 8'b0000_1111; end
              4'd7: begin fsel0 = 32'h1000_0000; seg0_we[7:0] = 8'b1100_0000; nxt_flit = 5'd13; seg_e = 3'b001;
                          fsel1 = 32'h0006_5432; seg1_we[7:0] = 8'b0001_1111; end
              4'd8: begin fsel0 = 32'h1000_0000; seg0_we[7:0] = 8'b1100_0000; nxt_flit = 5'd14; seg_e = 3'b001;
                          fsel1 = 32'h0076_5432; seg1_we[7:0] = 8'b0011_1111; end
              default: nxt_flit = 5'd06;
            endcase //qs2_cnt_ones
          end //d0
          5'd7: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd08; seg_e = 3'b001; end
              4'd2: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd09; seg_e = 3'b001;
                          fsel1 = 32'h0000_0001; seg1_we[7:0] = 8'b0000_0001; end
              4'd3: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd10; seg_e = 3'b001;
                          fsel1 = 32'h0000_0021; seg1_we[7:0] = 8'b0000_0011; end
              4'd4: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd11; seg_e = 3'b001;
                          fsel1 = 32'h0000_0321; seg1_we[7:0] = 8'b0000_0111; end
              4'd5: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd12; seg_e = 3'b001;
                          fsel1 = 32'h0000_4321; seg1_we[7:0] = 8'b0000_1111; end
              4'd6: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd13; seg_e = 3'b001;
                          fsel1 = 32'h0005_4321; seg1_we[7:0] = 8'b0001_1111; end
              4'd7: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd14; seg_e = 3'b001;
                          fsel1 = 32'h0065_4321; seg1_we[7:0] = 8'b0011_1111; end
              4'd8: begin fsel0 = 32'h0000_0000; seg0_we[7:0] = 8'b1000_0000; nxt_flit = 5'd15; seg_e = 3'b001;
                          fsel1 = 32'h0765_4321; seg1_we[7:0] = 8'b0111_1111; end
              default: nxt_flit = 5'd07;
            endcase //qs2_cnt_ones
          end //d0
          5'd8: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b0000_0001; nxt_flit = 5'd09; end
              4'd2: begin fsel1 = 32'h0000_0010; seg1_we[7:0] = 8'b0000_0001; nxt_flit = 5'd10; end
              4'd3: begin fsel1 = 32'h0000_0210; seg1_we[7:0] = 8'b0000_0111; nxt_flit = 5'd11; end
              4'd4: begin fsel1 = 32'h0000_3210; seg1_we[7:0] = 8'b0000_1111; nxt_flit = 5'd12; end
              4'd5: begin fsel1 = 32'h0004_3210; seg1_we[7:0] = 8'b0001_1111; nxt_flit = 5'd13; end
              4'd6: begin fsel1 = 32'h0054_3210; seg1_we[7:0] = 8'b0011_1111; nxt_flit = 5'd14; end
              4'd7: begin fsel1 = 32'h0654_3210; seg1_we[7:0] = 8'b1111_1111; nxt_flit = 5'd15; end
              4'd8: begin fsel1 = 32'h7654_3210; seg1_we[7:0] = 8'b1111_1111; nxt_flit = 5'd16; seg_e = 3'b010; end
              default: nxt_flit = 5'd08;
            endcase //qs2_cnt_ones
          end //d0
          5'd9: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b0000_0010; nxt_flit = 5'd10; end
              4'd2: begin fsel1 = 32'h0000_0100; seg1_we[7:0] = 8'b0000_0110; nxt_flit = 5'd11; end
              4'd3: begin fsel1 = 32'h0000_2100; seg1_we[7:0] = 8'b0000_1110; nxt_flit = 5'd12; end
              4'd4: begin fsel1 = 32'h0003_2100; seg1_we[7:0] = 8'b0001_1110; nxt_flit = 5'd13; end
              4'd5: begin fsel1 = 32'h0043_2100; seg1_we[7:0] = 8'b0011_1110; nxt_flit = 5'd14; end
              4'd6: begin fsel1 = 32'h0543_2100; seg1_we[7:0] = 8'b0111_1110; nxt_flit = 5'd15; end
              4'd7: begin fsel1 = 32'h6543_2100; seg1_we[7:0] = 8'b1111_1110; nxt_flit = 5'd16; seg_e = 3'b010; end
              4'd8: begin fsel1 = 32'h6543_2100; seg1_we[7:0] = 8'b1111_1110; nxt_flit = 5'd17; seg_e = 3'b010;
                          fsel2 = 32'h0000_0007; seg2_we[7:0] = 8'b0000_0001; end
              default: nxt_flit = 5'd09;
            endcase //qs2_cnt_ones
          end //d0
          5'd10: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b0000_0100; nxt_flit = 5'd11; end
              4'd2: begin fsel1 = 32'h0000_1000; seg1_we[7:0] = 8'b0000_1100; nxt_flit = 5'd12; end
              4'd3: begin fsel1 = 32'h0002_1000; seg1_we[7:0] = 8'b0001_1100; nxt_flit = 5'd13; end
              4'd4: begin fsel1 = 32'h0032_1000; seg1_we[7:0] = 8'b0011_1100; nxt_flit = 5'd14; end
              4'd5: begin fsel1 = 32'h0432_1000; seg1_we[7:0] = 8'b0111_1100; nxt_flit = 5'd15; end
              4'd6: begin fsel1 = 32'h5432_1000; seg1_we[7:0] = 8'b1111_1100; nxt_flit = 5'd16; seg_e = 3'b010; end
              4'd7: begin fsel1 = 32'h5432_1000; seg1_we[7:0] = 8'b1111_1100; nxt_flit = 5'd17; seg_e = 3'b010;
                          fsel2 = 32'h0000_0006; seg2_we[7:0] = 8'b0000_0001; end
              4'd8: begin fsel1 = 32'h5432_1000; seg1_we[7:0] = 8'b1111_1100; nxt_flit = 5'd18; seg_e = 3'b010;
                          fsel2 = 32'h0000_0076; seg2_we[7:0] = 8'b0000_0011; end
              default: nxt_flit = 5'd10;
            endcase //qs2_cnt_ones
          end //d0
          5'd11: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b0000_1000; nxt_flit = 5'd12; end
              4'd2: begin fsel1 = 32'h0001_0000; seg1_we[7:0] = 8'b0001_1000; nxt_flit = 5'd13; end
              4'd3: begin fsel1 = 32'h0021_0000; seg1_we[7:0] = 8'b0011_1000; nxt_flit = 5'd14; end
              4'd4: begin fsel1 = 32'h0321_0000; seg1_we[7:0] = 8'b0111_1000; nxt_flit = 5'd15; end
              4'd5: begin fsel1 = 32'h4321_0000; seg1_we[7:0] = 8'b1111_1000; nxt_flit = 5'd16; seg_e = 3'b010; end
              4'd6: begin fsel1 = 32'h4321_0000; seg1_we[7:0] = 8'b1111_1000; nxt_flit = 5'd17; seg_e = 3'b010;
                          fsel2 = 32'h0000_0005; seg2_we[7:0] = 8'b0000_0001; end
              4'd7: begin fsel1 = 32'h4321_0000; seg1_we[7:0] = 8'b1111_1000; nxt_flit = 5'd18; seg_e = 3'b010;
                          fsel2 = 32'h0000_0065; seg2_we[7:0] = 8'b0000_0011; end
              4'd8: begin fsel1 = 32'h4321_0000; seg1_we[7:0] = 8'b1111_1000; nxt_flit = 5'd19; seg_e = 3'b010;
                          fsel2 = 32'h0000_0765; seg2_we[7:0] = 8'b0000_0111; end
              default: nxt_flit = 5'd11;
            endcase //qs2_cnt_ones
          end //d0
          5'd12: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b0001_0000; nxt_flit = 5'd13; end
              4'd2: begin fsel1 = 32'h0010_0000; seg1_we[7:0] = 8'b0011_0000; nxt_flit = 5'd14; end
              4'd3: begin fsel1 = 32'h0210_0000; seg1_we[7:0] = 8'b0111_0000; nxt_flit = 5'd15; end
              4'd4: begin fsel1 = 32'h3210_0000; seg1_we[7:0] = 8'b1111_0000; nxt_flit = 5'd16; seg_e = 3'b010; end
              4'd5: begin fsel1 = 32'h3210_0000; seg1_we[7:0] = 8'b1111_0000; nxt_flit = 5'd17; seg_e = 3'b010;
                          fsel2 = 32'h0000_0004; seg2_we[7:0] = 8'b0000_0001; end
              4'd6: begin fsel1 = 32'h3210_0000; seg1_we[7:0] = 8'b1111_0000; nxt_flit = 5'd18; seg_e = 3'b010;
                          fsel2 = 32'h0000_0054; seg2_we[7:0] = 8'b0000_0011; end
              4'd7: begin fsel1 = 32'h3210_0000; seg1_we[7:0] = 8'b1111_0000; nxt_flit = 5'd19; seg_e = 3'b010;
                          fsel2 = 32'h0000_0654; seg2_we[7:0] = 8'b0000_0111; end
              4'd8: begin fsel1 = 32'h3210_0000; seg1_we[7:0] = 8'b1111_0000; nxt_flit = 5'd20; seg_e = 3'b010;
                          fsel2 = 32'h0000_7654; seg2_we[7:0] = 8'b0000_1111; end
              default: nxt_flit = 5'd12;
            endcase //qs2_cnt_ones
          end //d0
          5'd13: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b0010_0000; nxt_flit = 5'd14; end
              4'd2: begin fsel1 = 32'h0100_0000; seg1_we[7:0] = 8'b0110_0000; nxt_flit = 5'd15; end
              4'd3: begin fsel1 = 32'h2100_0000; seg1_we[7:0] = 8'b1110_0000; nxt_flit = 5'd16; seg_e = 3'b010; end
              4'd4: begin fsel1 = 32'h2100_0000; seg1_we[7:0] = 8'b1110_0000; nxt_flit = 5'd17; seg_e = 3'b010;
                          fsel2 = 32'h0000_0003; seg2_we[7:0] = 8'b0000_0001; end
              4'd5: begin fsel1 = 32'h2100_0000; seg1_we[7:0] = 8'b1110_0000; nxt_flit = 5'd18; seg_e = 3'b010;
                          fsel2 = 32'h0000_0043; seg2_we[7:0] = 8'b0000_0011; end
              4'd6: begin fsel1 = 32'h2100_0000; seg1_we[7:0] = 8'b1110_0000; nxt_flit = 5'd19; seg_e = 3'b010;
                          fsel2 = 32'h0000_0543; seg2_we[7:0] = 8'b0000_0111; end
              4'd7: begin fsel1 = 32'h2100_0000; seg1_we[7:0] = 8'b1110_0000; nxt_flit = 5'd20; seg_e = 3'b010;
                          fsel2 = 32'h0000_6543; seg2_we[7:0] = 8'b0000_1111; end
              4'd8: begin fsel1 = 32'h2100_0000; seg1_we[7:0] = 8'b1110_0000; nxt_flit = 5'd21; seg_e = 3'b010;
                          fsel2 = 32'h0007_6543; seg2_we[7:0] = 8'b0001_1111; end
              default: nxt_flit = 5'd13;
            endcase //qs2_cnt_ones
          end //d0 
          5'd14: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b0100_0000; nxt_flit = 5'd15; end
              4'd2: begin fsel1 = 32'h1000_0000; seg1_we[7:0] = 8'b1100_0000; nxt_flit = 5'd16; seg_e = 3'b010; end
              4'd3: begin fsel1 = 32'h1000_0000; seg1_we[7:0] = 8'b1100_0000; nxt_flit = 5'd17; seg_e = 3'b010;
                          fsel2 = 32'h0000_0002; seg2_we[7:0] = 8'b0000_0001; end
              4'd4: begin fsel1 = 32'h1000_0000; seg1_we[7:0] = 8'b1100_0000; nxt_flit = 5'd18; seg_e = 3'b010;
                          fsel2 = 32'h0000_0032; seg2_we[7:0] = 8'b0000_0011; end
              4'd5: begin fsel1 = 32'h1000_0000; seg1_we[7:0] = 8'b1100_0000; nxt_flit = 5'd19; seg_e = 3'b010;
                          fsel2 = 32'h0000_0432; seg2_we[7:0] = 8'b0000_0111; end
              4'd6: begin fsel1 = 32'h1000_0000; seg1_we[7:0] = 8'b1100_0000; nxt_flit = 5'd20; seg_e = 3'b010;
                          fsel2 = 32'h0000_5432; seg2_we[7:0] = 8'b0000_1111; end
              4'd7: begin fsel1 = 32'h1000_0000; seg1_we[7:0] = 8'b1100_0000; nxt_flit = 5'd21; seg_e = 3'b010;
                          fsel2 = 32'h0006_5432; seg2_we[7:0] = 8'b0001_1111; end
              4'd8: begin fsel1 = 32'h1000_0000; seg1_we[7:0] = 8'b1100_0000; nxt_flit = 5'd22; seg_e = 3'b010;
                          fsel2 = 32'h0076_5432; seg2_we[7:0] = 8'b0011_1111; end
              default: nxt_flit = 5'd14;
            endcase //qs2_cnt_ones
          end //d0
          5'd15: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd16; seg_e = 3'b010; end
              4'd2: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd17; seg_e = 3'b010;
                          fsel2 = 32'h0000_0001; seg2_we[7:0] = 8'b0000_0001; end
              4'd3: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd18; seg_e = 3'b010;
                          fsel2 = 32'h0000_0021; seg2_we[7:0] = 8'b0000_0011; end
              4'd4: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd19; seg_e = 3'b010;
                          fsel2 = 32'h0000_0321; seg2_we[7:0] = 8'b0000_0111; end
              4'd5: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd20; seg_e = 3'b010;
                          fsel2 = 32'h0000_4321; seg2_we[7:0] = 8'b0000_1111; end
              4'd6: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd21; seg_e = 3'b010;
                          fsel2 = 32'h0005_4321; seg2_we[7:0] = 8'b0001_1111; end
              4'd7: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd22; seg_e = 3'b010;
                          fsel2 = 32'h0065_4321; seg2_we[7:0] = 8'b0011_1111; end
              4'd8: begin fsel1 = 32'h0000_0000; seg1_we[7:0] = 8'b1000_0000; nxt_flit = 5'd23; seg_e = 3'b010;
                          fsel2 = 32'h0765_4321; seg2_we[7:0] = 8'b0111_1111; end
              default: nxt_flit = 5'd15;
            endcase //qs2_cnt_ones
          end //d0
          5'd16: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b0000_0001; nxt_flit = 5'd17; end
              4'd2: begin fsel2 = 32'h0000_0010; seg2_we[7:0] = 8'b0000_0001; nxt_flit = 5'd18; end
              4'd3: begin fsel2 = 32'h0000_0210; seg2_we[7:0] = 8'b0000_0111; nxt_flit = 5'd19; end
              4'd4: begin fsel2 = 32'h0000_3210; seg2_we[7:0] = 8'b0000_1111; nxt_flit = 5'd20; end
              4'd5: begin fsel2 = 32'h0004_3210; seg2_we[7:0] = 8'b0001_1111; nxt_flit = 5'd21; end
              4'd6: begin fsel2 = 32'h0054_3210; seg2_we[7:0] = 8'b0011_1111; nxt_flit = 5'd22; end
              4'd7: begin fsel2 = 32'h0654_3210; seg2_we[7:0] = 8'b1111_1111; nxt_flit = 5'd23; end
              4'd8: begin fsel2 = 32'h7654_3210; seg2_we[7:0] = 8'b1111_1111; nxt_flit =  5'd0; seg_e = 3'b100; end
              default: nxt_flit = 5'd16;
            endcase //qs2_cnt_ones
          end //d0
          5'd17: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b0000_0010; nxt_flit = 5'd18; end
              4'd2: begin fsel2 = 32'h0000_0100; seg2_we[7:0] = 8'b0000_0110; nxt_flit = 5'd19; end
              4'd3: begin fsel2 = 32'h0000_2100; seg2_we[7:0] = 8'b0000_1110; nxt_flit = 5'd20; end
              4'd4: begin fsel2 = 32'h0003_2100; seg2_we[7:0] = 8'b0001_1110; nxt_flit = 5'd21; end
              4'd5: begin fsel2 = 32'h0043_2100; seg2_we[7:0] = 8'b0011_1110; nxt_flit = 5'd22; end
              4'd6: begin fsel2 = 32'h0543_2100; seg2_we[7:0] = 8'b0111_1110; nxt_flit = 5'd23; end
              4'd7: begin fsel2 = 32'h6543_2100; seg2_we[7:0] = 8'b1111_1110; nxt_flit =  5'd0; seg_e = 3'b100; end
              4'd8: begin fsel2 = 32'h6543_2100; seg2_we[7:0] = 8'b1111_1110; nxt_flit = 5'd01; seg_e = 3'b100;
                          fsel0 = 32'h0000_0007; seg0_we[7:0] = 8'b0000_0001; end
              default: nxt_flit = 5'd17;
            endcase //qs2_cnt_ones
          end //d0
          5'd18: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b0000_0100; nxt_flit = 5'd19; end
              4'd2: begin fsel2 = 32'h0000_1000; seg2_we[7:0] = 8'b0000_1100; nxt_flit = 5'd20; end
              4'd3: begin fsel2 = 32'h0002_1000; seg2_we[7:0] = 8'b0001_1100; nxt_flit = 5'd21; end
              4'd4: begin fsel2 = 32'h0032_1000; seg2_we[7:0] = 8'b0011_1100; nxt_flit = 5'd22; end
              4'd5: begin fsel2 = 32'h0432_1000; seg2_we[7:0] = 8'b0111_1100; nxt_flit = 5'd23; end
              4'd6: begin fsel2 = 32'h5432_1000; seg2_we[7:0] = 8'b1111_1100; nxt_flit =  5'd0; seg_e = 3'b100; end
              4'd7: begin fsel2 = 32'h5432_1000; seg2_we[7:0] = 8'b1111_1100; nxt_flit = 5'd01; seg_e = 3'b100;
                          fsel0 = 32'h0000_0006; seg0_we[7:0] = 8'b0000_0001; end
              4'd8: begin fsel2 = 32'h5432_1000; seg2_we[7:0] = 8'b1111_1100; nxt_flit = 5'd02; seg_e = 3'b100;
                          fsel0 = 32'h0000_0076; seg0_we[7:0] = 8'b0000_0011; end
              default: nxt_flit = 5'd18;
            endcase //qs2_cnt_ones
          end //d0
          5'd19: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b0000_1000; nxt_flit = 5'd20; end
              4'd2: begin fsel2 = 32'h0001_0000; seg2_we[7:0] = 8'b0001_1000; nxt_flit = 5'd21; end
              4'd3: begin fsel2 = 32'h0021_0000; seg2_we[7:0] = 8'b0011_1000; nxt_flit = 5'd22; end
              4'd4: begin fsel2 = 32'h0321_0000; seg2_we[7:0] = 8'b0111_1000; nxt_flit = 5'd23; end
              4'd5: begin fsel2 = 32'h4321_0000; seg2_we[7:0] = 8'b1111_1000; nxt_flit =  5'd0; seg_e = 3'b100; end
              4'd6: begin fsel2 = 32'h4321_0000; seg2_we[7:0] = 8'b1111_1000; nxt_flit = 5'd01; seg_e = 3'b100;
                          fsel0 = 32'h0000_0005; seg0_we[7:0] = 8'b0000_0001; end
              4'd7: begin fsel2 = 32'h4321_0000; seg2_we[7:0] = 8'b1111_1000; nxt_flit = 5'd02; seg_e = 3'b100;
                          fsel0 = 32'h0000_0065; seg0_we[7:0] = 8'b0000_0011; end
              4'd8: begin fsel2 = 32'h4321_0000; seg2_we[7:0] = 8'b1111_1000; nxt_flit = 5'd03; seg_e = 3'b100;
                          fsel0 = 32'h0000_0765; seg0_we[7:0] = 8'b0000_0111; end
              default: nxt_flit = 5'd19;
            endcase //qs2_cnt_ones
          end //d0
          5'd20: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b0001_0000; nxt_flit = 5'd21; end
              4'd2: begin fsel2 = 32'h0010_0000; seg2_we[7:0] = 8'b0011_0000; nxt_flit = 5'd22; end
              4'd3: begin fsel2 = 32'h0210_0000; seg2_we[7:0] = 8'b0111_0000; nxt_flit = 5'd23; end
              4'd4: begin fsel2 = 32'h3210_0000; seg2_we[7:0] = 8'b1111_0000; nxt_flit =  5'd0; seg_e = 3'b100; end
              4'd5: begin fsel2 = 32'h3210_0000; seg2_we[7:0] = 8'b1111_0000; nxt_flit = 5'd01; seg_e = 3'b100;
                          fsel0 = 32'h0000_0004; seg0_we[7:0] = 8'b0000_0001; end
              4'd6: begin fsel2 = 32'h3210_0000; seg2_we[7:0] = 8'b1111_0000; nxt_flit = 5'd02; seg_e = 3'b100;
                          fsel0 = 32'h0000_0054; seg0_we[7:0] = 8'b0000_0011; end
              4'd7: begin fsel2 = 32'h3210_0000; seg2_we[7:0] = 8'b1111_0000; nxt_flit = 5'd03; seg_e = 3'b100;
                          fsel0 = 32'h0000_0654; seg0_we[7:0] = 8'b0000_0111; end
              4'd8: begin fsel2 = 32'h3210_0000; seg2_we[7:0] = 8'b1111_0000; nxt_flit = 5'd04; seg_e = 3'b100;
                          fsel0 = 32'h0000_7654; seg0_we[7:0] = 8'b0000_1111; end
              default: nxt_flit = 5'd20;
            endcase //qs2_cnt_ones
          end //d0
          5'd21: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b0010_0000; nxt_flit = 5'd22; end
              4'd2: begin fsel2 = 32'h0100_0000; seg2_we[7:0] = 8'b0110_0000; nxt_flit = 5'd23; end
              4'd3: begin fsel2 = 32'h2100_0000; seg2_we[7:0] = 8'b1110_0000; nxt_flit =  5'd0; seg_e = 3'b100; end
              4'd4: begin fsel2 = 32'h2100_0000; seg2_we[7:0] = 8'b1110_0000; nxt_flit = 5'd01; seg_e = 3'b100;
                          fsel0 = 32'h0000_0003; seg0_we[7:0] = 8'b0000_0001; end
              4'd5: begin fsel2 = 32'h2100_0000; seg2_we[7:0] = 8'b1110_0000; nxt_flit = 5'd02; seg_e = 3'b100;
                          fsel0 = 32'h0000_0043; seg0_we[7:0] = 8'b0000_0011; end
              4'd6: begin fsel2 = 32'h2100_0000; seg2_we[7:0] = 8'b1110_0000; nxt_flit = 5'd03; seg_e = 3'b100;
                          fsel0 = 32'h0000_0543; seg0_we[7:0] = 8'b0000_0111; end
              4'd7: begin fsel2 = 32'h2100_0000; seg2_we[7:0] = 8'b1110_0000; nxt_flit = 5'd04; seg_e = 3'b100;
                          fsel0 = 32'h0000_6543; seg0_we[7:0] = 8'b0000_1111; end
              4'd8: begin fsel2 = 32'h2100_0000; seg2_we[7:0] = 8'b1110_0000; nxt_flit = 5'd05; seg_e = 3'b100;
                          fsel0 = 32'h0007_6543; seg0_we[7:0] = 8'b0001_1111; end
              default: nxt_flit = 5'd21;
            endcase //qs2_cnt_ones
          end //d0 
          5'd22: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b0100_0000; nxt_flit = 5'd23; end
              4'd2: begin fsel2 = 32'h1000_0000; seg2_we[7:0] = 8'b1100_0000; nxt_flit =  5'd0; seg_e = 3'b100; end
              4'd3: begin fsel2 = 32'h1000_0000; seg2_we[7:0] = 8'b1100_0000; nxt_flit = 5'd01; seg_e = 3'b100;
                          fsel0 = 32'h0000_0002; seg0_we[7:0] = 8'b0000_0001; end
              4'd4: begin fsel2 = 32'h1000_0000; seg2_we[7:0] = 8'b1100_0000; nxt_flit = 5'd02; seg_e = 3'b100;
                          fsel0 = 32'h0000_0032; seg0_we[7:0] = 8'b0000_0011; end
              4'd5: begin fsel2 = 32'h1000_0000; seg2_we[7:0] = 8'b1100_0000; nxt_flit = 5'd03; seg_e = 3'b100;
                          fsel0 = 32'h0000_0432; seg0_we[7:0] = 8'b0000_0111; end
              4'd6: begin fsel2 = 32'h1000_0000; seg2_we[7:0] = 8'b1100_0000; nxt_flit = 5'd04; seg_e = 3'b100;
                          fsel0 = 32'h0000_5432; seg0_we[7:0] = 8'b0000_1111; end
              4'd7: begin fsel2 = 32'h1000_0000; seg2_we[7:0] = 8'b1100_0000; nxt_flit = 5'd05; seg_e = 3'b100;
                          fsel0 = 32'h0006_5432; seg0_we[7:0] = 8'b0001_1111; end
              4'd8: begin fsel2 = 32'h1000_0000; seg2_we[7:0] = 8'b1100_0000; nxt_flit = 5'd06; seg_e = 3'b100;
                          fsel0 = 32'h0076_5432; seg0_we[7:0] = 8'b0011_1111; end
              default: nxt_flit = 5'd22;
            endcase //qs2_cnt_ones
          end //d0
          5'd23: begin
            unique case(qs2_cnt_ones) inside  //qs2_cnt_ones has to be at least 1 to get here
              4'd1: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit =  5'd0; seg_e = 3'b100; end
              4'd2: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit = 5'd01; seg_e = 3'b100;
                          fsel0 = 32'h0000_0001; seg0_we[7:0] = 8'b0000_0001; end
              4'd3: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit = 5'd02; seg_e = 3'b100;
                          fsel0 = 32'h0000_0021; seg0_we[7:0] = 8'b0000_0011; end
              4'd4: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit = 5'd03; seg_e = 3'b100;
                          fsel0 = 32'h0000_0321; seg0_we[7:0] = 8'b0000_0111; end
              4'd5: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit = 5'd04; seg_e = 3'b100;
                          fsel0 = 32'h0000_4321; seg0_we[7:0] = 8'b0000_1111; end
              4'd6: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit = 5'd05; seg_e = 3'b100;
                          fsel0 = 32'h0005_4321; seg0_we[7:0] = 8'b0001_1111; end
              4'd7: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit = 5'd06; seg_e = 3'b100;
                          fsel0 = 32'h0065_4321; seg0_we[7:0] = 8'b0011_1111; end
              4'd8: begin fsel2 = 32'h0000_0000; seg2_we[7:0] = 8'b1000_0000; nxt_flit = 5'd07; seg_e = 3'b100;
                          fsel0 = 32'h0765_4321; seg0_we[7:0] = 8'b0111_1111; end
              default: nxt_flit = 5'd23;
            endcase //qs2_cnt_ones
          end //d0
        default: ;
      endcase //current_flit
      end //else
    end // rx_v
  end //always_comb

  
endmodule
