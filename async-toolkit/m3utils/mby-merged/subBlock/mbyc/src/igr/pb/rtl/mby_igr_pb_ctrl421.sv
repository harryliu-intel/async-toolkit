//------------------------------------------------------------------------------
//
//  INTEL CONFIDENTIAL
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
// -- Description  : This is Control block for the Packet Buffer(PB) block.
//                   This control can be configured for
//                   1X400G: 2x200G: 1x200G,1x100G,1x100G: 1x100G,1x100G,2x200G: 1x100G,1x100G,1x100G,1x100G
//                   It interfaces with dpc and epl_shim.
//------------------------------------------------------------------------------


module  mby_igr_pb_ctrl421
  import mby_igr_pkg::*;
(
  input  logic              cclk,
  input  logic               rst,  // synchronized warm reset
  input  logic [1:0] i_port_rate,
  input  dpc_pb_t       i_dpc_pb,
  
  input  logic [3:0]     i_pb_rd,  //read request for PB per logical port
  input  pb_shell_rdata_t      [PB_BANKS-1:0] i_pb_shell_rdata,
  input  pb_shell_rmd_t        [PB_BANKS-1:0] i_pb_shell_rmd,       
  output pb_shell_ctrl_wdata_t [PB_BANKS-1:0] o_pb_shell_ctrl_wdata,
  output pb_shell_ctrl_wmd_t   [PB_BANKS-1:0] o_pb_shell_ctrl_wmd,
  output dpc_pb_t [PB_BANKS-1:0] o_pb_shim //valid,md,data
);

 
  logic [2:0] rate, q_rate; //000=off,001=100G,010=200G,100=400G
  logic [PB_BANKS-1:0] w_bnk, nxt_w_bnk;
  logic [PB_BANKS-1:0] r_bnk, nxt_r_bnk;
  logic                wr_e;
  logic                rd_e;
  logic                rbnkiswbnk;
  logic [PB_BANKS-1:0] w_adrs_bnk_inc;
  logic [PB_BANKS-1:0] r_adrs_bnk_inc;
  logic [11:0] fifo_dpth4, fifo_dpth2, fifo_dpth1, fifo_dpth;
  logic        fifo_used_cnt_e;
  logic        fifo_used_cnt_inc;
  logic [11:0] fifo_used_cnt;
  logic        fifo_used_cnt_of, fifo_used_cnt_uf; //overflow,underflow
  logic        fifo_used_mt, fifo_used_full;
 
  logic [PB_BANK_ADRS-1:0] w_adrs_bnk     [PB_BANKS-1:0];
  logic [PB_BANK_ADRS-1:0] nxt_w_adrs_bnk [PB_BANKS-1:0];
  logic [PB_BANK_ADRS-1:0] r_adrs_bnk     [PB_BANKS-1:0];
  logic [PB_BANK_ADRS-1:0] nxt_r_adrs_bnk [PB_BANKS-1:0];
  logic [PB_BANK_ADRS-1:0] shell_mem_adrs_bnk [PB_BANKS-1:0]; // r/w address to mgm mem shell
  logic [PB_BANKS-1:0]     shell_mem_we_bnk;
  logic [PB_BANKS-1:0]     shell_mem_re_bnk;
  logic [PB_BANK_ADRS-1:0]  used_cnt_bnk0;
  logic [PB_BANK_ADRS-1:0]  used_cnt_bnk1;
  logic [PB_BANK_ADRS-1:0]  used_cnt_bnk2;
  logic [PB_BANK_ADRS-1:0]  used_cnt_bnk3;
 
  pb_shell_ctrl_wdata_t [PB_BANKS-1:0] pb_shell_ctrl_wdata;
  pb_shell_ctrl_wmd_t   [PB_BANKS-1:0] pb_shell_ctrl_wmd;
  pb_shell_rdata_t      [PB_BANKS-1:0] pb_shell_rdata;
  pb_shell_rmd_t        [PB_BANKS-1:0] pb_shell_rmd;
  dpc_pb_t              [PB_BANKS-1:0] pb_shim;
  dpc_pb_t              [PB_BANKS-1:0] q_pb_shim;

  assign fifo_dpth4 = 12'd4095;
  assign fifo_dpth2 = 12'd2047;
  assign fifo_dpth1 = 12'd1023;
  
  assign o_pb_shim = q_pb_shim;
  assign o_pb_shell_ctrl_wdata = pb_shell_ctrl_wdata;
  assign o_pb_shell_ctrl_wmd   = pb_shell_ctrl_wmd;

  generate
    genvar g_bnk;
    for(g_bnk=0; g_bnk<PB_BANKS; g_bnk++) begin: gen_pb_shim_ffs
      always_ff @(posedge cclk) begin
        if(rst) begin
          q_pb_shim[g_bnk].tsmd <= '0;//FIXME just reset valid??
          q_pb_shim[g_bnk].d    <= '0;
        end
        else if(pb_shim[g_bnk].v) begin
          q_pb_shim[g_bnk].tsmd <= pb_shim[g_bnk].tsmd;
          q_pb_shim[g_bnk].d    <= pb_shim[g_bnk].d;
        end
      end
      always_ff @(posedge cclk) q_pb_shim[g_bnk].v <= pb_shim[g_bnk].v;
    end //for

    for(g_bnk=0; g_bnk<PB_BANKS; g_bnk++) begin: gen_pb2shell  
      always_ff @(posedge cclk) begin
        if(rst) begin
          pb_shell_ctrl_wdata[g_bnk].rd_en <= '0;
          pb_shell_ctrl_wdata[g_bnk].wr_en <= '0;
          pb_shell_ctrl_wdata[g_bnk].adr   <= '0;
          pb_shell_ctrl_wmd[g_bnk].rd_en   <= '0;
          pb_shell_ctrl_wmd[g_bnk].wr_en   <= '0;
          pb_shell_ctrl_wmd[g_bnk].adr     <= '0;
        end
        else if(shell_mem_we_bnk[g_bnk] || shell_mem_re_bnk[g_bnk]) begin
          pb_shell_ctrl_wdata[g_bnk].rd_en <= shell_mem_re_bnk[g_bnk];
          pb_shell_ctrl_wdata[g_bnk].wr_en <= shell_mem_we_bnk[g_bnk];
          pb_shell_ctrl_wdata[g_bnk].adr   <= shell_mem_adrs_bnk[g_bnk];
          pb_shell_ctrl_wmd[g_bnk].rd_en   <= shell_mem_re_bnk[g_bnk];
          pb_shell_ctrl_wmd[g_bnk].wr_en   <= shell_mem_we_bnk[g_bnk];
          pb_shell_ctrl_wmd[g_bnk].adr     <= shell_mem_adrs_bnk[g_bnk];
        end
      end  
      always_ff @(posedge cclk) begin
        if(rst) begin
          pb_shell_ctrl_wdata[g_bnk].wr_data <= '0;
          pb_shell_ctrl_wmd[g_bnk].wr_data   <= '0;
        end
        else if(shell_mem_we_bnk[g_bnk]) begin
          pb_shell_ctrl_wdata[g_bnk].wr_data <= i_dpc_pb.d;
          pb_shell_ctrl_wmd[g_bnk].wr_data   <= {10'b0, i_dpc_pb.tsmd}; //FIXME add ecc
        end
      end
    end //for
    
//rd_data frm shell input ffs
    for(g_bnk=0; g_bnk<PB_BANKS; g_bnk++) begin: gen_shell_rdata_ffs
      always_ff @(posedge cclk) begin
        if(rst) begin
          pb_shell_rdata[g_bnk].rd_data <= '0;  //FIXME just reset valid??
          pb_shell_rmd[g_bnk].rd_data   <= '0;  //FIXME just reset valid??
        end          
        else if(i_pb_shell_rdata[g_bnk].rd_valid) begin
          pb_shell_rdata[g_bnk].rd_data <= i_pb_shell_rdata[g_bnk].rd_data;
          pb_shell_rmd[g_bnk].rd_data <= i_pb_shell_rmd[g_bnk].rd_data;
        end
      end
      always_ff @(posedge cclk) pb_shell_rdata[g_bnk].rd_valid <= i_pb_shell_rdata[g_bnk].rd_valid;
      always_ff @(posedge cclk) pb_shell_rmd[g_bnk].rd_valid   <= i_pb_shell_rmd[g_bnk].rd_valid;
    end //for
    
  endgenerate
  
  
  always_comb begin
    unique case(i_port_rate) inside
      2'b01:   begin rate = 3'b001; fifo_dpth = fifo_dpth1; end
      2'b10:   begin rate = 3'b010; fifo_dpth = fifo_dpth2; end
      2'b11:   begin rate = 3'b100; fifo_dpth = fifo_dpth4; end
      default: begin rate = 3'b000; fifo_dpth = fifo_dpth4; end
    endcase    
  end
  always_ff @(posedge cclk) q_rate <= rate;
  
// bank write and read ffs
  always_ff @(posedge cclk) w_bnk <= (rst)? 4'b0001: nxt_w_bnk;
  always_ff @(posedge cclk) r_bnk <= (rst)? 4'b0001: nxt_r_bnk;

//fifo used cnt write read address pointers per bank
//FIXME should overflow be a packet valid from dpc and the fifo is full
//FIXME should underflow be a packet read intp bp and the fifo is empty

  always_ff @(posedge cclk) begin
    if(rst) begin
      fifo_used_cnt    <= '0;
      fifo_used_cnt_of <= '0;
      fifo_used_cnt_uf <= '0;
    end
    else if(fifo_used_cnt_e) begin
      if(fifo_used_cnt_inc)
        fifo_used_cnt <= fifo_used_cnt + 12'h001;
      else
        fifo_used_cnt <= fifo_used_cnt + 12'hfff;          
    end //fifo_used_cnt_e          
  end

  generate
    genvar g_i, g_j;
    for(g_i=0; g_i<PB_BANKS; g_i++) begin: gen_w_adrs_bnk
      always_ff @(posedge cclk) begin
        if(rst)                      w_adrs_bnk[g_i] <= '0;
        else if(w_adrs_bnk_inc[g_i]) w_adrs_bnk[g_i] <= nxt_w_adrs_bnk[g_i];
      end
    end//for
    for(g_j=0; g_j<PB_BANKS; g_j++) begin: gen_r_adrs_bnk
      always_ff @(posedge cclk) begin
        if(rst)                      r_adrs_bnk[g_j] <= '0;
        else if(r_adrs_bnk_inc[g_j]) r_adrs_bnk[g_j] <= nxt_r_adrs_bnk[g_j];
      end
    end//for
  endgenerate
  
  always_comb begin
    pb_shim[0] = '0;
    pb_shim[1] = '0;
    pb_shim[2] = '0;
    pb_shim[3] = '0;
    w_adrs_bnk_inc = '0;
    r_adrs_bnk_inc = '0;
    shell_mem_we_bnk = '0;
    shell_mem_re_bnk = '0;
    nxt_w_bnk = w_bnk;
    nxt_r_bnk = r_bnk;
    nxt_w_adrs_bnk = w_adrs_bnk;
    nxt_r_adrs_bnk = r_adrs_bnk;
    shell_mem_adrs_bnk = r_adrs_bnk;  
    
    fifo_used_mt   = ~| fifo_used_cnt;
    fifo_used_full = (fifo_used_cnt == fifo_dpth);
    
    wr_e           = i_dpc_pb.v & (~ fifo_used_full);
    rbnkiswbnk     = (r_bnk == w_bnk);
//FIXME rd_e is currently 400G port0, needed for all ports and rates per port
    rd_e           = i_pb_rd[0] & (~ fifo_used_mt) &
                     (((~ wr_e) & rbnkiswbnk) | (~ rbnkiswbnk));

     
    fifo_used_cnt_e = wr_e ^ rd_e;
    fifo_used_cnt_inc = wr_e; 
    
    if(q_rate[2]) begin  //400G port
      if(wr_e) begin
        w_adrs_bnk_inc  = w_bnk;
        if(w_bnk[0]) begin
          shell_mem_we_bnk[0]   = 1'b1;
          shell_mem_adrs_bnk[0] = w_adrs_bnk[0];
          nxt_w_adrs_bnk[0]     = w_adrs_bnk[0] + 10'h001;
        end
        if(w_bnk[1]) begin
          shell_mem_we_bnk[1]   = 1'b1;
          shell_mem_adrs_bnk[1] = w_adrs_bnk[1];
          nxt_w_adrs_bnk[1]     = w_adrs_bnk[1] + 10'h001;
        end
        if(w_bnk[2]) begin
          shell_mem_we_bnk[2]   = 1'b1;
          shell_mem_adrs_bnk[2] = w_adrs_bnk[2];
          nxt_w_adrs_bnk[2]     = w_adrs_bnk[2] + 10'h001;
        end
        if(w_bnk[3]) begin
          shell_mem_we_bnk[3]   = 1'b1;
          shell_mem_adrs_bnk[3] = w_adrs_bnk[3];
          nxt_w_adrs_bnk[3]     = w_adrs_bnk[3] + 10'h001;
        end
        nxt_w_bnk       = (w_bnk << 1);
      end //wr_e
      
      if(rd_e) begin
        r_adrs_bnk_inc  = r_bnk;
        if(r_bnk[0]) begin
          shell_mem_re_bnk[0] = 1'b1;
          nxt_r_adrs_bnk[0]   = r_adrs_bnk[0] + 10'h001;
        end
        if(r_bnk[1]) begin
          shell_mem_re_bnk[1] = 1'b1;
          nxt_r_adrs_bnk[1]   = r_adrs_bnk[1] + 10'h001;
        end
        if(r_bnk[2]) begin
          shell_mem_re_bnk[2] = 1'b1;
          nxt_r_adrs_bnk[2]   = r_adrs_bnk[2] + 10'h001;
        end
        if(r_bnk[3]) begin
          shell_mem_re_bnk[2] = 1'b1;
          nxt_r_adrs_bnk[3] = r_adrs_bnk[3] + 10'h001;
        end
        nxt_r_bnk       = (r_bnk << 1);        
      end //rd_e
      
      //pb to shim rd data from mem
      //output rd data to shim on port0 for 400G
      pb_shim[0].v =  pb_shell_rdata[0].rd_valid | pb_shell_rdata[1].rd_valid |
                      pb_shell_rdata[2].rd_valid | pb_shell_rdata[3].rd_valid;
      //FIXME correct size and ecc for tsmd
      pb_shim[0].tsmd = ({62{pb_shell_rdata[0].rd_valid}} & pb_shell_rdata[0].rd_data[637:576]) |
                        ({62{pb_shell_rdata[1].rd_valid}} & pb_shell_rdata[1].rd_data[637:576]) |
                        ({62{pb_shell_rdata[2].rd_valid}} & pb_shell_rdata[2].rd_data[637:576]) |
                        ({62{pb_shell_rdata[3].rd_valid}} & pb_shell_rdata[3].rd_data[637:576]);
      pb_shim[0].d =    ({576{pb_shell_rdata[0].rd_valid}} & pb_shell_rdata[0].rd_data[575:0]) |
                        ({576{pb_shell_rdata[1].rd_valid}} & pb_shell_rdata[1].rd_data[575:0]) |
                        ({576{pb_shell_rdata[2].rd_valid}} & pb_shell_rdata[2].rd_data[575:0]) |
                        ({576{pb_shell_rdata[3].rd_valid}} & pb_shell_rdata[3].rd_data[575:0]);
              
    end //if 400G port
    
    
  end //always_comb
      

endmodule
