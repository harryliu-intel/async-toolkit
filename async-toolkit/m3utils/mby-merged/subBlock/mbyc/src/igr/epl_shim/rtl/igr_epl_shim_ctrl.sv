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



module igr_epl_shim_ctrl 
  import mby_igr_pkg::*;
(

  input logic cclk,
  input logic rst,
  
// EPL I/O from MBY FS Dataplane Interface signals.
  input logic         rx_port_e,  
  input logic [7:0]   rx_data_v,
  input epl_md_t      rx_md,     //from s1 ff
  
  output epl_md_t     o_seg0_md,
  output epl_md_t     o_seg1_md,
  output epl_md_t     o_seg2_md,
  
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
  epl_md_t    s2q_rx_md;  //2 segments may be written simutaneously with different metadata.
  epl_md_t     seg0_md;  //2 segments may be written simutaneously with different metadata.
  epl_md_t     seg1_md;
  epl_md_t     seg2_md;
  logic  [7:0] seg0_we; //write enable for segment0 flits[7:0]
  logic  [7:0] seg1_we; //write enable for segment0 flits[15:8]
  logic  [7:0] seg2_we; //write enable for segment0 flits[23:16]
  logic  [2:0] seg_e;
  logic [31:0] fsel0;//EPL flit data select aligned data segment 0, each 4 bits is a sel field 4'b1000 is pad
  logic [31:0] fsel1;//EPL flit data select aligned data segment 0, each 4 bits is a sel field 4'b1000 is pad
  logic [31:0] fsel2;//EPL flit data select aligned data segment 0, each 4 bits is a sel field 4'b1000 is pad
  
  logic          sop_eop;
  logic       s2q_sop_eop;
  logic          eop_sop;
  logic       s2q_eop_sop;
  logic             rx_v;
  logic          s2q_rx_v;
  logic            sop_v;
  logic         s2q_sop_v;
  logic         flit_err; // the current flit count is incorrect set this bit.
  
//data, md may have sop that is written to a segemnt but segment is not written to PB.
//need to store sop state and write when segment is written to PB, the md for this flit will not have Sop
// set for the current segment.
//FIXME are there other md fields that need to be held until segment write and how to split along
// segment boundaries????
//FIXME WHAT ABOUT fcs_hint????
  logic      seg0_sop_e; //SOP in md for segment but segment not written to PB, hold until segment written
  logic      seg1_sop_e; //SOP in md for segment but segment not written to PB, hold until segment written
  logic      seg2_sop_e; //SOP in md for segment but segment not written to PB, hold until segment written
  logic      seg0_sop; //SOP in md for segment but segment not written to PB, hold until segment written
  logic      seg1_sop; //SOP in md for segment but segment not written to PB, hold until segment written
  logic      seg2_sop; //SOP in md for segment but segment not written to PB, hold until segment written

  shim_seg_pos_t new_eop_pos; 
  
  logic [7:0]      sflit;  //sop flit
  logic [7:0]   s2q_sflit;  //sop flit
  logic [7:0]      eflit;  //eop flit
  logic [7:0]   s2q_eflit;  //eop flit
  
  logic [3:0]    cnt_ones;
  logic [3:0] s2q_cnt_ones;

  shimfsel_t   s3q_seg0_sel;
  shimfsel_t   s3q_seg1_sel;
  shimfsel_t   s3q_seg2_sel;
  logic [7:0]  s3q_seg0_we;
  logic [7:0]  s3q_seg1_we;
  logic [7:0]  s3q_seg2_we;
  logic [2:0]  s3q_seg_e; // enable aligned segment for write to PB
  epl_md_t     s3q_seg0_md;
  epl_md_t     s3q_seg1_md;
  epl_md_t     s3q_seg2_md; 
  
    
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
//outputs end
  
  //desigmware count_ones
  parameter width = 8;  //used by function
  logic [3:0] nc_cnt_ones;  //noconnect only need cnt_ones 0to8
  `include "DW_dp_count_ones_function.inc"
  assign {nc_cnt_ones, cnt_ones} = DWF_dp_count_ones(rx_data_v);
  
   always_ff @(posedge cclk) current_flit <= (rst)? '0: nxt_flit;
   
   always_ff @(posedge cclk) begin
     seg0_sop <= (rst)? '0: (seg0_sop_e)? s2q_rx_md.sop: seg0_sop;
     seg1_sop <= (rst)? '0: (seg1_sop_e)? s2q_rx_md.sop: seg1_sop;
     seg2_sop <= (rst)? '0: (seg2_sop_e)? s2q_rx_md.sop: seg2_sop;
   end
   
//FIXME assertion never (rx_md.sop & rx_md.eop) & (rx_md.sop_pos == rx_md.eop_pos)

  assign rx_v  = |rx_data_v;
  assign eflit = (rx_md.eop_pos == 3'h0)? 8'b0000_0001:
                 (rx_md.eop_pos == 3'h1)? 8'b0000_0010:
                 (rx_md.eop_pos == 3'h2)? 8'b0000_0100:
                 (rx_md.eop_pos == 3'h3)? 8'b0000_1000:
                 (rx_md.eop_pos == 3'h4)? 8'b0001_0000:
                 (rx_md.eop_pos == 3'h5)? 8'b0010_0000:
                 (rx_md.eop_pos == 3'h6)? 8'b0100_0000: 8'b1000_0000;
  assign sflit = (rx_md.sop_pos == 3'h0)? 8'b0000_0001:
                 (rx_md.sop_pos == 3'h1)? 8'b0000_0010:
                 (rx_md.sop_pos == 3'h2)? 8'b0000_0100:
                 (rx_md.sop_pos == 3'h3)? 8'b0000_1000:
                 (rx_md.sop_pos == 3'h4)? 8'b0001_0000:
                 (rx_md.sop_pos == 3'h5)? 8'b0010_0000:
                 (rx_md.sop_pos == 3'h6)? 8'b0100_0000: 8'b1000_0000;
  assign sop_eop = (rx_md.sop_pos < s2q_rx_md.eop_pos);
  assign eop_sop = (rx_md.sop_pos > s2q_rx_md.eop_pos);
  assign sop_v   = (rx_v) & rx_md.sop;

//s2
  always_ff @(posedge cclk) s2q_rx_v    <= rx_v;
  always_ff @(posedge cclk) s2q_cnt_ones <= cnt_ones;
  always_ff @(posedge cclk) s2q_eflit   <= eflit;
  always_ff @(posedge cclk) s2q_sflit   <= sflit;
  always_ff @(posedge cclk) s2q_sop_eop <= sop_eop;
  always_ff @(posedge cclk) s2q_eop_sop <= eop_sop;
  always_ff @(posedge cclk) s2q_sop_v   <= sop_v;
  
  always_ff @(posedge cclk) s2q_rx_md <= (rst)? '0: (rx_v && rx_port_e)? rx_md: s2q_rx_md;
  
  always_comb
  begin
    seg0_md = s2q_rx_md;
    seg1_md = s2q_rx_md;
    seg2_md = s2q_rx_md;
    seg0_md.sop_pos = 3'h0;  //out of shim sop always in flit[0]
    seg1_md.sop_pos = 3'h0;  //out of shim sop always in flit[0]
    seg2_md.sop_pos = 3'h0;  //out of shim sop always in flit[0]
    new_eop_pos     = '0;  

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
    if(s2q_rx_v) begin
      if(s2q_rx_md.eop) begin //this case handles unaligned segments with valid EOP 
      unique case(current_flit) inside  //FIXME shoud current_flit be a 1-hot vector[23:0]??
        5'd0: begin
          seg0_we[7:0] = 8'b1111_1111;
          seg_e        = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment 
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h8888_8880;
              new_eop_pos.seg0 = 3'd0;
              seg_e            = 3'b001;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h8888_8810; //FIXME anything to do with FCS_hints???
              new_eop_pos.seg0 = 3'd1;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h8888_8210;
              new_eop_pos.seg0 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h8888_3210;
              new_eop_pos.seg0 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h8884_3210;
              new_eop_pos.seg0 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h8854_3210;
              new_eop_pos.seg0 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h8654_3210;
              new_eop_pos.seg0 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h7654_3210;
              new_eop_pos.seg0 = 3'd7;
              nxt_flit         = 5'd08;
              flit_err = s2q_sop_v & (~s2q_sflit[0]); //If there is a valid Sop and Eop is in flit[7] it has to be in flit[0]
            end                        
          endcase //reverse
        end //d0
        5'd1: begin
          seg0_we[7:0] = 8'b1111_1110;
          seg_e         = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h8888_8800;
              new_eop_pos.seg0 = 3'd1;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h8888_8100;
              new_eop_pos.seg0 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h8888_2100;
              new_eop_pos.seg0 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h8883_2100;
              new_eop_pos.seg0 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h8843_2100;
              new_eop_pos.seg0 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h8543_2100;
              new_eop_pos.seg0 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h6543_2100;
              new_eop_pos.seg0 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h6543_2100;
              fsel1            = 32'h8888_8887;
              new_eop_pos.seg1 = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err         = s2q_sop_v; //There should not be a valid Sop 
            end                         
          endcase //reverse
        end //d1
        5'd2: begin
          seg0_we[7:0] = 8'b1111_1100;
          seg_e        = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h8888_8000;
              new_eop_pos.seg0 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h8888_1000;
              new_eop_pos.seg0 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h8882_1000;
              new_eop_pos.seg0 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h8832_1000;
              new_eop_pos.seg0 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h8432_1000;
              new_eop_pos.seg0 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h5432_1000;
              new_eop_pos.seg0 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h5432_1000;
              fsel1            = 32'h8888_8886;
              new_eop_pos.seg1 = 3'd0;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h5432_1000;
              fsel1            = 32'h8888_8876;
              new_eop_pos.seg1 = 3'd1;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse          
        end //d2
        5'd3: begin
          seg0_we[7:0] = 8'b1111_1000;
          seg_e        = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h8888_0000;
              new_eop_pos.seg0 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h8881_0000;
              new_eop_pos.seg0 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h8821_0000;
              new_eop_pos.seg0 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h8321_0000;
              new_eop_pos.seg0 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h4321_0000;
              new_eop_pos.seg0 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h4321_0000;
              fsel1            = 32'h8888_8885;
              new_eop_pos.seg1 = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h4321_0000;
              fsel1            = 32'h8888_8865;
              new_eop_pos.seg1 = 3'd1;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h4321_0000;
              fsel1            = 32'h8888_8765;
              new_eop_pos.seg1 = 3'd2;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d3          
        5'd4: begin
          seg0_we[7:0] = 8'b1111_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h8880_0000;
              new_eop_pos.seg0 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h8810_0000;
              new_eop_pos.seg0 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h8210_0000;
              new_eop_pos.seg0 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h3210_0000;
              new_eop_pos.seg0 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_8884;
              new_eop_pos.seg1 = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_8854;
              new_eop_pos.seg1 = 3'd1;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_8654;
              new_eop_pos.seg1 = 3'd2;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h3210_0000;
              fsel1            = 32'h8888_7654;
              new_eop_pos.seg1 = 3'd3;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d4
        5'd5: begin
          seg0_we[7:0] = 8'b1110_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h8800_0000;
              new_eop_pos.seg0 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h8100_0000;
              new_eop_pos.seg0 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h2100_0000;
              new_eop_pos.seg0 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h2100_0000;
              fsel1            = 32'h8888_8883;
              new_eop_pos.seg1 = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h2100_0000;
              fsel1            = 32'h8888_8843;
              new_eop_pos.seg1 = 3'd1;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h2100_0000;
              fsel1            = 32'h8888_8543;
              new_eop_pos.seg1 = 3'd2;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h2100_0000;
              fsel1            = 32'h8888_6543;
              new_eop_pos.seg1 = 3'd3;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h2100_0000;
              fsel1            = 32'h8887_6543;
              new_eop_pos.seg1 = 3'd4;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d5
        5'd6: begin
          seg0_we[7:0] = 8'b1100_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h8000_0000;
              new_eop_pos.seg0 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h1000_0000;
              new_eop_pos.seg0 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_8882;
              new_eop_pos.seg1 = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_8832;
              new_eop_pos.seg1 = 3'd1;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_8432;
              new_eop_pos.seg1 = 3'd2;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8888_5432;
              new_eop_pos.seg1 = 3'd3;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8886_5432;
              new_eop_pos.seg1 = 3'd4;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h1000_0000;
              fsel1            = 32'h8876_5432;
              new_eop_pos.seg1 = 3'd5;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d6
        5'd7: begin
          seg0_we[7:0] = 8'b1000_0000;
          seg_e        = 3'b001;
          seg1_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel0            = 32'h0000_0000;
              new_eop_pos.seg0 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel1 = 32'h0000_0001; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel1 = 32'h0000_0021; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel1 = 32'h0000_0321; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel1 = 32'h0000_4321; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel1 = 32'h0005_4321; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel1 = 32'h0065_4321; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel1 = 32'h0765_4321; seg1_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_8881;
              new_eop_pos.seg1 = 3'd0;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_8821;
              new_eop_pos.seg1 = 3'd1;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_8321;
              new_eop_pos.seg1 = 3'd2;
              seg1_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8888_4321;
              new_eop_pos.seg1 = 3'd3;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8885_4321;
              new_eop_pos.seg1 = 3'd4;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8865_4321;
              new_eop_pos.seg1 = 3'd5;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel0            = 32'h0000_0000;
              fsel1            = 32'h8765_4321;
              new_eop_pos.seg1 = 3'd6;
              seg1_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd16;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d7
//end of seg0
        5'd8: begin
          seg1_we[7:0] = 8'b1111_1111;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h8888_8880;
              new_eop_pos.seg1 = 3'd0;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h8888_8810; //FIXME anything to do with FCS_hints???
              new_eop_pos.seg1 = 3'd1;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h8888_8210;
              new_eop_pos.seg1 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h8888_3210;
              new_eop_pos.seg1 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h8884_3210;
              new_eop_pos.seg1 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h8854_3210;
              new_eop_pos.seg1 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h8654_3210;
              new_eop_pos.seg1 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h7654_3210;
              new_eop_pos.seg1 = 3'd7;
              nxt_flit         = 5'd16;
              flit_err = s2q_sop_v & (~s2q_sflit[0]); //If there is a valid Sop and Eop is in flit[7] it has to be in flit[0]
            end                        
          endcase //reverse
        end //d8
        5'd9: begin
          seg1_we[7:0] = 8'b1111_1110;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h8888_8800;
              new_eop_pos.seg1 = 3'd1;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h8888_8100;
              new_eop_pos.seg1 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h8888_2100;
              new_eop_pos.seg1 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h8883_2100;
              new_eop_pos.seg1 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h8843_2100;
              new_eop_pos.seg1 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h8543_2100;
              new_eop_pos.seg1 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h6543_2100;
              new_eop_pos.seg1 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel2 = 32'h0000_0007; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h6543_2100;
              fsel2            = 32'h8888_8887;
              new_eop_pos.seg2 = 3'd0;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err         = s2q_sop_v; //There should not be a valid Sop 
            end                         
          endcase //reverse
        end //d9
        5'd10: begin
          seg1_we[7:0] = 8'b1111_1100;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h8888_8000;
              new_eop_pos.seg1 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h8888_1000;
              new_eop_pos.seg1 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h8882_1000;
              new_eop_pos.seg1 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h8832_1000;
              new_eop_pos.seg1 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h8432_1000;
              new_eop_pos.seg1 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h5432_1000;
              new_eop_pos.seg1 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel2 = 32'h0000_0006; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd8: begin fsel2 = 32'h0000_0076; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h5432_1000;
              fsel2            = 32'h8888_8886;
              new_eop_pos.seg2 = 3'd0;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd1; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h5432_1000;
              fsel2            = 32'h8888_8876;
              new_eop_pos.seg2 = 3'd1;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse          
        end //d10
        5'd11: begin
          seg1_we[7:0] = 8'b1111_1000;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h8888_0000;
              new_eop_pos.seg1 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h8881_0000;
              new_eop_pos.seg1 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h8821_0000;
              new_eop_pos.seg1 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h8321_0000;
              new_eop_pos.seg1 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h4321_0000;
              new_eop_pos.seg1 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel2 = 32'h0000_0005; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd7: begin fsel2 = 32'h0000_0065; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd8: begin fsel2 = 32'h0000_0765; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h4321_0000;
              fsel2            = 32'h8888_8885;
              new_eop_pos.seg2 = 3'd0;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h4321_0000;
              fsel2            = 32'h8888_8865;
              new_eop_pos.seg2 = 3'd1;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h4321_0000;
              fsel2            = 32'h8888_8765;
              new_eop_pos.seg2 = 3'd2;
              seg2_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d11          
        5'd12: begin
          seg1_we[7:0] = 8'b1111_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h8880_0000;
              new_eop_pos.seg1 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h8810_0000;
              new_eop_pos.seg1 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h8210_0000;
              new_eop_pos.seg1 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h3210_0000;
              new_eop_pos.seg1 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel2 = 32'h0000_0004; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd6: begin fsel2 = 32'h0000_0054; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd7: begin fsel2 = 32'h0000_0654; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd8: begin fsel2 = 32'h0000_7654; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_8884;
              new_eop_pos.seg2 = 3'd0;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_8854;
              new_eop_pos.seg2 = 3'd1;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_8654;
              new_eop_pos.seg2 = 3'd2;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h3210_0000;
              fsel2            = 32'h8888_7654;
              new_eop_pos.seg2 = 3'd3;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b011;
              nxt_flit         = 5'd0;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d12
        5'd13: begin
          seg1_we[7:0] = 8'b1110_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h8800_0000;
              new_eop_pos.seg1 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd15; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h8100_0000;
              new_eop_pos.seg1 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h2100_0000;
              new_eop_pos.seg1 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel2 = 32'h0000_0003; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel2 = 32'h0000_0043; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel2 = 32'h0000_0543; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel2 = 32'h0000_6543; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel2 = 32'h0007_6543; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_8883;
              new_eop_pos.seg2 = 3'd0;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_8843;
              new_eop_pos.seg2 = 3'd1;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_8543;
              new_eop_pos.seg2 = 3'd2;
              seg2_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b011;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8888_6543;
              new_eop_pos.seg2 = 3'd3;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h2100_0000;
              fsel2            = 32'h8887_6543;
              new_eop_pos.seg2 = 3'd4;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;
              nxt_flit         = 5'd0;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d13
        5'd14: begin
          seg1_we[7:0] = 8'b1100_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h8000_0000;
              new_eop_pos.seg1 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h1000_0000;
              new_eop_pos.seg1 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel2 = 32'h0000_0002; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd4: begin fsel2 = 32'h0000_0032; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd5: begin fsel2 = 32'h0000_0432; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd6: begin fsel2 = 32'h0000_5432; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd7: begin fsel2 = 32'h0006_5432; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd8: begin fsel2 = 32'h0076_5432; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8888_8882;
              new_eop_pos.seg2 = 3'd0;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8888_8832;
              new_eop_pos.seg2 = 3'd1;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8888_8432;
              new_eop_pos.seg2 = 3'd2;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8888_5432;
              new_eop_pos.seg2 = 3'd3;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8886_5432;
              new_eop_pos.seg2 = 3'd4;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h1000_0000;
              fsel2            = 32'h8876_5432;
              new_eop_pos.seg2 = 3'd5;
              seg2_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b110;
              nxt_flit         = 5'd0;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d14
        5'd15: begin
          seg1_we[7:0] = 8'b1000_0000;
          seg_e        = 3'b010;
          seg2_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel1            = 32'h0000_0000;
              new_eop_pos.seg1 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel2 = 32'h0000_0001; seg2_we[6:0] = 7'b000_0001; nxt_flit = 5'd17; end
                4'd3: begin fsel2 = 32'h0000_0021; seg2_we[6:0] = 7'b000_0011; nxt_flit = 5'd18; end
                4'd4: begin fsel2 = 32'h0000_0321; seg2_we[6:0] = 7'b000_0111; nxt_flit = 5'd19; end
                4'd5: begin fsel2 = 32'h0000_4321; seg2_we[6:0] = 7'b000_1111; nxt_flit = 5'd20; end
                4'd6: begin fsel2 = 32'h0005_4321; seg2_we[6:0] = 7'b001_1111; nxt_flit = 5'd21; end
                4'd7: begin fsel2 = 32'h0065_4321; seg2_we[6:0] = 7'b011_1111; nxt_flit = 5'd22; end
                4'd8: begin fsel2 = 32'h0765_4321; seg2_we[6:0] = 7'b111_1111; nxt_flit = 5'd23; end
                default: nxt_flit = 5'd16;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8888_8881;
              new_eop_pos.seg2 = 3'd0;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8888_8821;
              new_eop_pos.seg2 = 3'd1;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8888_8321;
              new_eop_pos.seg2 = 3'd2;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8888_4321;
              new_eop_pos.seg2 = 3'd3;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8885_4321;
              new_eop_pos.seg2 = 3'd4;
              seg2_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8865_4321;
              new_eop_pos.seg2 = 3'd5;
              seg2_we[7:0]   = 8'b1111_1111;
              seg_e            = 3'b110;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel1            = 32'h0000_0000;
              fsel2            = 32'h8765_4321;
              new_eop_pos.seg2 = 3'd6;
              seg2_we[7:0]  = 8'b1111_1111;
              seg_e            = 3'b110;
              nxt_flit         = 5'd0;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d15
//end of seg1
        5'd16: begin
          seg2_we[7:0] = 8'b1111_1111;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h8888_8880;
              new_eop_pos.seg2 = 3'd0;
              seg_e            = 3'b100;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h8888_8810; //FIXME anything to do with FCS_hints???
              new_eop_pos.seg2 = 3'd1;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h8888_8210;
              new_eop_pos.seg2 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h8888_3210;
              new_eop_pos.seg2 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h8884_3210;
              new_eop_pos.seg2 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h8854_3210;
              new_eop_pos.seg2 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h8654_3210;
              new_eop_pos.seg2 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h7654_3210;
              new_eop_pos.seg2 = 3'd7;
              nxt_flit         = 5'd0;
              flit_err = s2q_sop_v & (~s2q_sflit[0]); //If there is a valid Sop and Eop is in flit[7] it has to be in flit[0]
            end                        
          endcase //reverse
        end //d16
        5'd17: begin
          seg2_we[7:0] = 8'b1111_1110;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h8888_8800;
              new_eop_pos.seg2 = 3'd1;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h8888_8100;
              new_eop_pos.seg2 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h8888_2100;
              new_eop_pos.seg2 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h8883_2100;
              new_eop_pos.seg2 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h8843_2100;
              new_eop_pos.seg2 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h8543_2100;
              new_eop_pos.seg2 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h6543_2100;
              new_eop_pos.seg2 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel0 = 32'h0000_0007; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h6543_2100;
              fsel0            = 32'h8888_8887;
              new_eop_pos.seg0 = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err         = s2q_sop_v; //There should not be a valid Sop 
            end                         
          endcase //reverse
        end //d17
        5'd18: begin
          seg2_we[7:0] = 8'b1111_1100;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h8888_8000;
              new_eop_pos.seg2 = 3'd2;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h8888_1000;
              new_eop_pos.seg2 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h8882_1000;
              new_eop_pos.seg2 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h8832_1000;
              new_eop_pos.seg2 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h8432_1000;
              new_eop_pos.seg2 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h5432_1000;
              new_eop_pos.seg2 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel0 = 32'h0000_0006; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd8: begin fsel0 = 32'h0000_0076; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h5432_1000;
              fsel0            = 32'h8888_8886;
              new_eop_pos.seg0 = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h5432_1000;
              fsel0            = 32'h8888_8876;
              new_eop_pos.seg0 = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse          
        end //d18
        5'd19: begin
          seg2_we[7:0] = 8'b1111_1000;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h8888_0000;
              new_eop_pos.seg2 = 3'd3;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h8881_0000;
              new_eop_pos.seg2 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h8821_0000;
              new_eop_pos.seg2 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h8321_0000;
              new_eop_pos.seg2 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h4321_0000;
              new_eop_pos.seg2 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel0 = 32'h0000_0005; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd7: begin fsel0 = 32'h0000_0065; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd8: begin fsel0 = 32'h0000_0765; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h4321_0000;
              fsel0            = 32'h8888_8885;
              new_eop_pos.seg0 = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h4321_0000;
              fsel0            = 32'h8888_8865;
              new_eop_pos.seg0 = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h4321_0000;
              fsel0            = 32'h8888_8765;
              new_eop_pos.seg0 = 3'd2;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d19          
        5'd20: begin
          seg2_we[7:0] = 8'b1111_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h8880_0000;
              new_eop_pos.seg2 = 3'd4;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h8810_0000;
              new_eop_pos.seg2 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h8210_0000;
              new_eop_pos.seg2 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h3210_0000;
              new_eop_pos.seg2 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel0 = 32'h0000_0004; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd6: begin fsel0 = 32'h0000_0054; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd7: begin fsel0 = 32'h0000_0654; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd8: begin fsel0 = 32'h0000_7654; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h3210_0000;
              fsel0            = 32'h8888_8884;
              new_eop_pos.seg0 = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h3210_0000;
              fsel0            = 32'h8888_8854;
              new_eop_pos.seg0 = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h3210_0000;
              fsel0            = 32'h8888_8654;
              new_eop_pos.seg0 = 3'd2;
              seg0_we[7:0]      = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h3210_0000;
              fsel0            = 32'h8888_7654;
              new_eop_pos.seg0 = 3'd3;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d20
        5'd21: begin
          seg2_we[7:0] = 8'b1110_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h8800_0000;
              new_eop_pos.seg2 = 3'd5;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h8100_0000;
              new_eop_pos.seg2 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h2100_0000;
              new_eop_pos.seg2 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel0 = 32'h0000_0003; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd5: begin fsel0 = 32'h0000_0043; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd6: begin fsel0 = 32'h0000_0543; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd7: begin fsel0 = 32'h0000_6543; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd8: begin fsel0 = 32'h0007_6543; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h2100_0000;
              fsel0            = 32'h8888_8883;
              new_eop_pos.seg0 = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h2100_0000;
              fsel0            = 32'h8888_8843;
              new_eop_pos.seg0 = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h2100_0000;
              fsel0            = 32'h8888_8543;
              new_eop_pos.seg0 = 3'd2;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h2100_0000;
              fsel0            = 32'h8888_6543;
              new_eop_pos.seg0 = 3'd3;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h2100_0000;
              fsel0            = 32'h8887_6543;
              new_eop_pos.seg0 = 3'd4;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d21
        5'd22: begin
          seg2_we[7:0] = 8'b1100_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h8000_0000;
              new_eop_pos.seg2 = 3'd6;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h1000_0000;
              new_eop_pos.seg2 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel0 = 32'h0000_0002; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd4: begin fsel0 = 32'h0000_0032; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd5: begin fsel0 = 32'h0000_0432; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd6: begin fsel0 = 32'h0000_5432; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd7: begin fsel0 = 32'h0006_5432; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd8: begin fsel0 = 32'h0076_5432; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h1000_0000;
              fsel0            = 32'h8888_8882;
              new_eop_pos.seg0 = 3'd0;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h1000_0000;
              fsel0            = 32'h8888_8832;
              new_eop_pos.seg0 = 3'd1;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h1000_0000;
              fsel0            = 32'h8888_8432;
              new_eop_pos.seg0 = 3'd2;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h1000_0000;
              fsel0            = 32'h8888_5432;
              new_eop_pos.seg0 = 3'd3;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h1000_0000;
              fsel0            = 32'h8886_5432;
              new_eop_pos.seg0 = 3'd4;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h1000_0000;
              fsel0            = 32'h8876_5432;
              new_eop_pos.seg0 = 3'd5;
              seg0_we[7:0]    = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d22
        5'd23: begin
          seg2_we[7:0] = 8'b1000_0000;
          seg_e        = 3'b100;
          seg0_sop_e   = 1'b1;  //capture sop md for next segment
          unique case(1'b1) inside
            s2q_eflit[0]: begin
              fsel2            = 32'h0000_0000;
              new_eop_pos.seg2 = 3'd7;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 1 to get here
                4'd2: begin fsel0 = 32'h0000_0001; seg0_we[6:0] = 7'b000_0001; nxt_flit = 5'd01; end
                4'd3: begin fsel0 = 32'h0000_0021; seg0_we[6:0] = 7'b000_0011; nxt_flit = 5'd02; end
                4'd4: begin fsel0 = 32'h0000_0321; seg0_we[6:0] = 7'b000_0111; nxt_flit = 5'd03; end
                4'd5: begin fsel0 = 32'h0000_4321; seg0_we[6:0] = 7'b000_1111; nxt_flit = 5'd04; end
                4'd6: begin fsel0 = 32'h0005_4321; seg0_we[6:0] = 7'b001_1111; nxt_flit = 5'd05; end
                4'd7: begin fsel0 = 32'h0065_4321; seg0_we[6:0] = 7'b011_1111; nxt_flit = 5'd06; end
                4'd8: begin fsel0 = 32'h0765_4321; seg0_we[6:0] = 7'b111_1111; nxt_flit = 5'd07; end
                default: nxt_flit = 5'd0;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & (s2q_sflit[0] | ((s2q_cnt_ones > 4'd1) & ~s2q_sflit[1]));
            end
            s2q_eflit[1]: begin
              fsel2            = 32'h0000_0000;
              fsel0            = 32'h8888_8881;
              new_eop_pos.seg0 = 3'd0;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 2 to get here
                4'd3: begin fsel1 = 32'h0000_0002; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd4: begin fsel1 = 32'h0000_0032; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd5: begin fsel1 = 32'h0000_0432; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd6: begin fsel1 = 32'h0000_5432; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd7: begin fsel1 = 32'h0006_5432; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                4'd8: begin fsel1 = 32'h0076_5432; seg1_we[6:0] = 7'b011_1111; nxt_flit = 5'd14; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[1:0]) | ((s2q_cnt_ones > 4'd2) & ~s2q_sflit[2]));
            end
            s2q_eflit[2]: begin
              fsel2            = 32'h0000_0000;
              fsel0            = 32'h8888_8821;
              new_eop_pos.seg0 = 3'd1;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 3 to get here
                4'd4: begin fsel1 = 32'h0000_0003; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd5: begin fsel1 = 32'h0000_0043; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd6: begin fsel1 = 32'h0000_0543; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd7: begin fsel1 = 32'h0000_6543; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                4'd8: begin fsel1 = 32'h0007_6543; seg1_we[6:0] = 7'b001_1111; nxt_flit = 5'd13; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[2:0]) | ((s2q_cnt_ones > 4'd3) & ~s2q_sflit[3]));
            end
            s2q_eflit[3]: begin
              fsel2            = 32'h0000_0000;
              fsel0            = 32'h8888_8321;
              new_eop_pos.seg0 = 3'd2;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 4 to get here
                4'd5: begin fsel1 = 32'h0000_0004; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd6: begin fsel1 = 32'h0000_0054; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd7: begin fsel1 = 32'h0000_0654; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                4'd8: begin fsel1 = 32'h0000_7654; seg1_we[6:0] = 7'b000_1111; nxt_flit = 5'd12; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[3:0]) | ((s2q_cnt_ones > 4'd4) & ~s2q_sflit[4]));
            end            
            s2q_eflit[4]: begin
              fsel2            = 32'h0000_0000;
              fsel0            = 32'h8888_4321;
              new_eop_pos.seg0 = 3'd3;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;              
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 5 to get here
                4'd6: begin fsel1 = 32'h0000_0005; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd7: begin fsel1 = 32'h0000_0065; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                4'd8: begin fsel1 = 32'h0000_0765; seg1_we[6:0] = 7'b000_0111; nxt_flit = 5'd11; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[4:0]) | ((s2q_cnt_ones > 4'd5) & ~s2q_sflit[5]));
            end            
            s2q_eflit[5]: begin
              fsel2            = 32'h0000_0000;
              fsel0            = 32'h8885_4321;
              new_eop_pos.seg0 = 3'd4;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 6 to get here
                4'd7: begin fsel1 = 32'h0000_0006; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end
                4'd8: begin fsel1 = 32'h0000_0076; seg1_we[6:0] = 7'b000_0011; nxt_flit = 5'd10; end
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[5:0]) | ((s2q_cnt_ones > 4'd6) & ~s2q_sflit[6]));
            end            
            s2q_eflit[6]: begin
              fsel2            = 32'h0000_0000;
              fsel0            = 32'h8865_4321;
              new_eop_pos.seg0 = 3'd5;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;
              unique case(s2q_cnt_ones) inside  //s2q_cnt_ones has to be at least 7 to get here
                4'd8: begin fsel1 = 32'h0000_0007; seg1_we[6:0] = 7'b000_0001; nxt_flit = 5'd09; end  //FIXME this shold be Sop does sop md need to be held
                default: nxt_flit = 5'd08;
              endcase //s2q_cnt_ones
              flit_err = s2q_sop_v & ((|s2q_sflit[6:0]) | ((s2q_cnt_ones > 4'd7) & ~s2q_sflit[7]));
            end            
            s2q_eflit[7]: begin
              fsel2            = 32'h0000_0000;
              fsel0            = 32'h8765_4321;
              new_eop_pos.seg0 = 3'd6;
              seg0_we[7:0]     = 8'b1111_1111;
              seg_e            = 3'b101;
              nxt_flit         = 5'd08;
              flit_err = s2q_sop_v; //There should not be a valid Sop 
            end                        
          endcase //reverse
        end //d23
//end of seg2
      endcase //current_flit
      end //if(eop)
      else begin  //no EOP so the unaligned flit is full sop should only then be in flits 0, 8, 16
        unique case(current_flit) inside  //FIXME shoud current_flit be a 1-hot vector[23:0]??
          5'd0: begin
            seg0_we[7:0] = 8'b1111_1111;
            seg_e        = 3'b001;
            nxt_flit    = 5'd08;    
          end //d0
          5'd1: begin
            seg0_we[7:0] = 8'b1111_1110;
            seg1_we[7:0] = 8'b0000_0001;
            seg_e        = 3'b001;
            nxt_flit    = 5'd09;    
          end //d0
          5'd2: begin
            seg0_we[7:0] = 8'b1111_1100;
            seg1_we[7:0] = 8'b0000_0011;
            seg_e        = 3'b001;
            nxt_flit    = 5'd10;    
          end //d0
          5'd3: begin
            seg0_we[7:0] = 8'b1111_1000;
            seg1_we[7:0] = 8'b0000_0111;
            seg_e        = 3'b001;
            nxt_flit    = 5'd11;    
          end //d0
          5'd4: begin
            seg0_we[7:0] = 8'b1111_0000;
            seg1_we[7:0] = 8'b0000_1111;
            seg_e        = 3'b001;
            nxt_flit    = 5'd12;    
          end //d0
          5'd5: begin
            seg0_we[7:0] = 8'b1110_0000;
            seg1_we[7:0] = 8'b0001_1111;
            seg_e        = 3'b001;
            nxt_flit    = 5'd13;    
          end //d0
          5'd6: begin
            seg0_we[7:0] = 8'b1100_0000;
            seg1_we[7:0] = 8'b0011_1111;
            seg_e        = 3'b001;
            nxt_flit    = 5'd14;    
          end //d0
          5'd7: begin
            seg0_we[7:0] = 8'b1000_0000;
            seg1_we[7:0] = 8'b0111_1111;
            seg_e        = 3'b001;
            nxt_flit    = 5'd15;    
          end //d0
          5'd8: begin
            seg1_we[7:0] = 8'b1111_1111;
            seg_e        = 3'b010;
            nxt_flit    = 5'd16;    
          end //d0
          5'd9: begin
            seg1_we[7:0] = 8'b1111_1110;
            seg2_we[7:0] = 8'b0000_0001;
            seg_e        = 3'b010;
            nxt_flit    = 5'd17;    
          end //d0
          5'd10: begin
            seg1_we[7:0] = 8'b1111_1100;
            seg2_we[7:0] = 8'b0000_0011;
            seg_e        = 3'b010;
            nxt_flit    = 5'd18;    
          end //d0
          5'd11: begin
            seg1_we[7:0] = 8'b1111_1000;
            seg2_we[7:0] = 8'b0000_0111;
            seg_e        = 3'b010;
            nxt_flit    = 5'd19;    
          end //d0
          5'd12: begin
            seg1_we[7:0] = 8'b1111_0000;
            seg2_we[7:0] = 8'b0000_1111;
            seg_e        = 3'b010;
            nxt_flit    = 5'd20;    
          end //d0
          5'd13: begin
            seg1_we[7:0] = 8'b1110_0000;
            seg2_we[7:0] = 8'b0001_1111;
            seg_e        = 3'b010;
            nxt_flit    = 5'd21;    
          end //d0
          5'd14: begin
            seg1_we[7:0] = 8'b1100_0000;
            seg2_we[7:0] = 8'b0011_1111;
            seg_e        = 3'b010;
            nxt_flit    = 5'd22;
          end //d0
          5'd15: begin
            seg1_we[7:0] = 8'b1000_0000;
            seg2_we[7:0] = 8'b0111_1111;
            seg_e        = 3'b010;
            nxt_flit    = 5'd23;    
          end //d0
          5'd16: begin
            seg2_we[7:0] = 8'b1111_1111;
            seg_e        = 3'b100;
            nxt_flit    = 5'd0;    
          end //d0
          5'd17: begin
            seg2_we[7:0] = 8'b1111_1110;
            seg0_we[7:0] = 8'b0000_0001;
            seg_e        = 3'b100;
            nxt_flit    = 5'd01;    
          end //d0
          5'd18: begin
            seg2_we[7:0] = 8'b1111_1100;
            seg0_we[7:0] = 8'b0000_0011;
            seg_e        = 3'b100;
            nxt_flit    = 5'd02;    
          end //d0
          5'd19: begin
            seg2_we[7:0] = 8'b1111_1000;
            seg0_we[7:0] = 8'b0000_0111;
            seg_e        = 3'b100;
            nxt_flit    = 5'd03;    
          end //d0
          5'd20: begin
            seg2_we[7:0] = 8'b1111_0000;
            seg0_we[7:0] = 8'b0000_1111;
            seg_e        = 3'b100;
            nxt_flit    = 5'd04;    
          end //d0
          5'd21: begin
            seg2_we[7:0] = 8'b1110_0000;
            seg0_we[7:0] = 8'b0001_1111;
            seg_e        = 3'b100;
            nxt_flit    = 5'd05;    
         end //d0
          5'd22: begin
            seg2_we[7:0] = 8'b1100_0000;
            seg0_we[7:0] = 8'b0011_1111;
            seg_e        = 3'b100;
            nxt_flit    = 5'd06;    
         end //d0
          5'd23: begin
            seg2_we[7:0] = 8'b1000_0000;
            seg0_we[7:0] = 8'b0111_1111;
            seg_e        = 3'b100;
            nxt_flit    = 5'd07;    
          end //d0
          default: ;
        endcase      
      end //else
    end // rx_v
  end //always_comb

  
  

endmodule
