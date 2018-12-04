//
//  Copyright 2018 - 2028 Intel Corporation All Rights Reserved.
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
// -- Author       : John Lo          
// -- Project Name : Madison Bay (MBY)
// -- Description  : 
//
// EPL uses a:d this corresponds to 0:3 in EGR.
//------------------------------------------------------------------------------


/**************************************************************
 * File Name    : egr_tcu_epl_shim.sv
 * Author Name  : John Lo
 * Description  : 
 * Parent Module: 
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-11-22
 * Notes        : cclk, hreset_n, sreset_n, egr_reset_n
 *
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_epl_shim (
  // epl_if  	
  input  logic [1:0]                    tx_enable_port_num, // Indicator of logical port associated with the tx_enable signal.
  input  logic                          tx_data_enable,     // Indicator of whether EPL is ready to accept tx data.
  output logic                          tx_data_valid_resp ,// tx_data_enable response for EPL credit.
  output logic [1:0]                    tx_port_num,        // Indicator of logical port the data and metadata is associated with for each EPL TX channel                   
  output logic [23:0]                   tx_metadata,        // Per Fllit metadata
                                        //     18 -- PTP Snap   Timestamp snap when SoP is being transmitted
                                        //  17:16 -- ERROR      Bad Packet (append bad FCS to packet)
                                        //     15 -- Rsv        Reserved
                                        //     14 -- EOP        End of Packet   
                                        //  13:11 -- EOP Pos    Flit containing EOP Undefined if no EOP in clock cycle
                                        //  10:8  -- Byte Pos   Location of most significant valid byte in EOP flit Undefined if no EOP in clock cycle
                                        //      7 -- SOP        Start of Packet 
                                        //    6:4 -- SOP Pos    Flit containing SOP 
                                        //    3:0 -- TC         TC As described in MAC Receive Traffic Classes Undefined if no SOP in clock cycle
  output logic [7:0]                    tx_ecc,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  output logic [7:0]                    tx_data_valid,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  output logic [71:0]                   tx0_data_w_ecc, // Transmit data bus is 8 flits with ECC protection:[71:64] = ECC covers Tx data [63:0] 
  output logic [71:0]                   tx1_data_w_ecc, // NOTE:   SOP is always placed in the lowest flit. 
  output logic [71:0]                   tx2_data_w_ecc, //  
  output logic [71:0]                   tx3_data_w_ecc, //  
  output logic [71:0]                   tx4_data_w_ecc, //  
  output logic [71:0]                   tx5_data_w_ecc, //  
  output logic [71:0]                   tx6_data_w_ecc, //  
  output logic [71:0]                   tx7_data_w_ecc, //  
  // egr_pkt_mod_if
  output logic                          buf_ren_port0, // to packet scheduler
  output logic                          buf_ren_port1, // to packet scheduler
  output logic                          buf_ren_port2, // to packet scheduler
  output logic                          buf_ren_port3, // to packet scheduler 
  input  logic [23:0]                   tx_metadata_port0,        // Per Fllit metadata
  input  logic [23:0]                   tx_metadata_port1,        // Per Fllit metadata
  input  logic [23:0]                   tx_metadata_port2,        // Per Fllit metadata
  input  logic [23:0]                   tx_metadata_port3,        // Per Fllit metadata
  input  logic [7:0]                    tx_ecc_port0,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [7:0]                    tx_ecc_port1,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [7:0]                    tx_ecc_port2,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [7:0]                    tx_ecc_port3,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [7:0]                    tx_data_valid_port0,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [7:0]                    tx_data_valid_port1,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [7:0]                    tx_data_valid_port2,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [7:0]                    tx_data_valid_port3,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [7:0][71:0]              tx_data_w_ecc_port0,  // 
  input  logic [7:0][71:0]              tx_data_w_ecc_port1,  // 
  input  logic [7:0][71:0]              tx_data_w_ecc_port2,  // 
  input  logic [7:0][71:0]              tx_data_w_ecc_port3,  // 
  // epl_pkt_scheduler_if
  input  logic                          tc_rdy_port0,
  input  logic                          tc_rdy_port1,
  input  logic                          tc_rdy_port2,
  input  logic                          tc_rdy_port3,
  // csr_if
  output logic                          port_num_err,          // to csr status bit
  input  logic [3:0]                    port_config,   // 0: 4ch, 1: 1 ch, 2: 2 ch, 3: rsvd
  // global signals
  input  logic 	                        reset_n,
  input  logic 	                        clk   
  );

 
  logic [1:0]                    tx_enable_port_num_d ;
  logic                          tx_data_enable_d     ; 
  logic                          x1port_4ch           ; // TDM tx_port_num  0 ...
  logic                          x2port_2ch           ; // TDM tx_port_num  0,2 ...
  logic                          x4port_1ch           ; // TDM tx_port_num  0,1,2,3 ...
  logic                          x3port_12ch          ; // TDM tx_port_num  0,2,1,2 ...
  logic                          x3port_21ch          ; // TDM tx_port_num  0,2,0,3 ...
  logic [7:0][71:0]              tx_data_w_ecc        ; 
 
  parameter  PORT0 = 2'b00;
  parameter  PORT1 = 2'b01;
  parameter  PORT2 = 2'b10;
  parameter  PORT3 = 2'b11;

  typedef enum logic [1:0] {TDM_SLOT0            = 2'b00 
                           ,TDM_SLOT1            = 2'b01
                           ,TDM_SLOT2            = 2'b10 
                           ,TDM_SLOT3            = 2'b11
                           } statetype;

  statetype                state, nx_state ;
  logic                    nx_tx_data_valid; 
  logic  [1:0]             nx_tx_port_num  ; 
  logic  [23:0]            nx_tx_metadata  ; 
  logic  [7:0]             nx_tx_ecc       ; 
  logic  [7:0][71:0]       nx_tx_data_w_ecc;

  // begin egr_tcu_epl_shim
  always_comb begin : EGR_EPL_SHIM_SM_COMB
       nx_state          = state;
       buf_ren_port0     = '0;
       buf_ren_port1     = '0;
       buf_ren_port2     = '0;
       buf_ren_port3     = '0;
       nx_tx_data_valid  = '0; 
       nx_tx_port_num    = '0; 
       nx_tx_metadata    = '0; 
       nx_tx_ecc         = '0; 
       nx_tx_data_w_ecc  = '0;
       port_num_err      = '0; 
    case (state)
      TDM_SLOT0: begin
          nx_state          = x1port_4ch ? TDM_SLOT0 : TDM_SLOT1;
          // 
          nx_state          = TDM_SLOT0;
          buf_ren_port0     = tx_data_enable_d && (tx_enable_port_num_d==PORT0) && tc_rdy_port0;
          nx_tx_data_valid  = tx_data_valid_port0; // 8bits 
          nx_tx_port_num    = PORT0;
          nx_tx_metadata    = tx_metadata_port0;  
          nx_tx_ecc         = tx_ecc_port0     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port0;
          port_num_err      = tx_enable_port_num_d!=PORT0; 
      end // TDM_SLOT0 
      TDM_SLOT1: begin
          nx_state          = TDM_SLOT2;
        if (x2port_2ch | x3port_12ch | x3port_21ch) begin
          buf_ren_port2     = tx_data_enable_d && (tx_enable_port_num_d==PORT2) && tc_rdy_port2;
          nx_tx_data_valid  = tx_data_valid_port2; // 8bits
          nx_tx_port_num    = PORT2;
          nx_tx_metadata    = tx_metadata_port2;  
          nx_tx_ecc         = tx_ecc_port2     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port2;
          port_num_err      = tx_enable_port_num_d!=PORT2; 
        end // if (x2port_2ch)
        else if (x4port_1ch) begin
          buf_ren_port1     = tx_data_enable_d && (tx_enable_port_num_d==PORT1) && tc_rdy_port1;
          nx_tx_data_valid  = tx_data_valid_port1; // 8bits
          nx_tx_port_num    = PORT1;
          nx_tx_metadata    = tx_metadata_port1;  
          nx_tx_ecc         = tx_ecc_port1     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port1;
          port_num_err      = tx_enable_port_num_d!=PORT1; 
        end // if (x4port_1ch)
        else  begin
          buf_ren_port0     = tx_data_enable_d && (tx_enable_port_num_d==PORT0) && tc_rdy_port0;
          nx_tx_data_valid  = tx_data_valid_port0; // 8bits 
          nx_tx_port_num    = PORT0;
          nx_tx_metadata    = tx_metadata_port0;  
          nx_tx_ecc         = tx_ecc_port0     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port0;
          port_num_err      = tx_enable_port_num_d!=PORT0; 
       end // if (x4port_1ch)
      end // TDM_SLOT1 
      TDM_SLOT2: begin
          nx_state          = TDM_SLOT3;
        if (x2port_2ch | x3port_21ch) begin
          buf_ren_port0     = tx_data_enable_d && (tx_enable_port_num_d==PORT0) && tc_rdy_port0;
          nx_tx_data_valid  = tx_data_valid_port0; // 8bits
          nx_tx_port_num    = PORT0;
          nx_tx_metadata    = tx_metadata_port0;  
          nx_tx_ecc         = tx_ecc_port0     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port0;
          port_num_err      = tx_enable_port_num_d!=PORT0; 
        end
        else if (x4port_1ch) begin
          buf_ren_port2     = tx_data_enable_d && (tx_enable_port_num_d==PORT2) && tc_rdy_port2;
          nx_tx_data_valid  = tx_data_valid_port2; // 8bits
          nx_tx_port_num    = PORT2;
          nx_tx_metadata    = tx_metadata_port2;  
          nx_tx_ecc         = tx_ecc_port2     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port2;
          port_num_err      = tx_enable_port_num_d!=PORT2; 
        end // if (x4port_1ch)
        else if (x3port_12ch) begin
          buf_ren_port1     = tx_data_enable_d && (tx_enable_port_num_d==PORT1) && tc_rdy_port1;
          nx_tx_data_valid  = tx_data_valid_port1; // 8bits
          nx_tx_port_num    = PORT1;
          nx_tx_metadata    = tx_metadata_port1;  
          nx_tx_ecc         = tx_ecc_port1     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port1;
          port_num_err      = tx_enable_port_num_d!=PORT1; 
        end // if (x4port_1ch)
        else  begin
          buf_ren_port0     = tx_data_enable_d && (tx_enable_port_num_d==PORT0) && tc_rdy_port0;
          nx_tx_data_valid  = tx_data_valid_port0; // 8bits 
          nx_tx_port_num    = PORT0;
          nx_tx_metadata    = tx_metadata_port0;  
          nx_tx_ecc         = tx_ecc_port0     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port0;
          port_num_err      = tx_enable_port_num_d!=PORT0; 
        end // 
      end // TDM_SLOT2 
      TDM_SLOT3: begin
          nx_state          = TDM_SLOT0;
        if (x2port_2ch | x3port_12ch) begin
          buf_ren_port2     = tx_data_enable_d && (tx_enable_port_num_d==PORT2) && tc_rdy_port2;
          nx_tx_data_valid  = tx_data_valid_port2; // 8bits
          nx_tx_port_num    = PORT2;
          nx_tx_metadata    = tx_metadata_port2;  
          nx_tx_ecc         = tx_ecc_port2     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port2;
          port_num_err      = tx_enable_port_num_d!=PORT2; 
        end
        else if (x4port_1ch | x3port_21ch) begin
          buf_ren_port3     = tx_data_enable_d && (tx_enable_port_num_d==PORT3) && tc_rdy_port3;
          nx_tx_data_valid  = tx_data_valid_port3; // 8bits
          nx_tx_port_num    = PORT3;
          nx_tx_metadata    = tx_metadata_port3;  
          nx_tx_ecc         = tx_ecc_port3     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port3;
          port_num_err      = tx_enable_port_num_d!=PORT3; 
        end // if (x4port_1ch)
        else  begin
          buf_ren_port0     = tx_data_enable_d && (tx_enable_port_num_d==PORT0) && tc_rdy_port0;
          nx_tx_data_valid  = tx_data_valid_port0; // 8bits 
          nx_tx_port_num    = PORT0;
          nx_tx_metadata    = tx_metadata_port0;  
          nx_tx_ecc         = tx_ecc_port0     ;  
       	  nx_tx_data_w_ecc  = tx_data_w_ecc_port0;
          port_num_err      = tx_enable_port_num_d!=PORT0; 
        end // 
      end // TDM_SLOT3
      default:    nx_state          = TDM_SLOT0;
    endcase // end case (state) 
  end : EGR_EPL_SHIM_SM_COMB
 
  // seq part
  always_ff @ (posedge clk) begin 
    if (!reset_n) begin
      state          <= TDM_SLOT0;
      tx_data_valid  <= '0; 
      tx_port_num    <= '0;
      tx_metadata    <= '0;
      tx_ecc         <= '0;
      tx_data_w_ecc  <= '0;
    end
    else begin
      state          <= nx_state;
      tx_data_valid  <= nx_tx_data_valid ; 
      tx_port_num    <= nx_tx_port_num   ;
      tx_metadata    <= nx_tx_metadata   ;
      tx_ecc         <= nx_tx_ecc        ;
      tx_data_w_ecc  <= nx_tx_data_w_ecc ;
    end
  end 
 
  /***** glue logic *****/
  always_ff @ (posedge clk) 
    if (!reset_n) begin
      tx_enable_port_num_d      <= 1'b0;
      tx_data_enable_d          <= 1'b0;
      tx_data_valid_resp        <= 1'b0;
      x1port_4ch  <= '0;
      x2port_2ch  <= '0;
      x4port_1ch  <= '0;
      x3port_12ch <= '0;
      x3port_21ch <= '0;
    end  
    else begin
      tx_enable_port_num_d      <= tx_enable_port_num;
      tx_data_enable_d          <= tx_data_enable;
      tx_data_valid_resp        <= tx_data_enable_d;
      x1port_4ch  <= port_config[3:0] == 4'h0;
      x2port_2ch  <= port_config[3:0] == 4'h1;
      x4port_1ch  <= port_config[3:0] == 4'h2;
      x3port_12ch <= port_config[3:0] == 4'h3;
      x3port_21ch <= port_config[3:0] == 4'h4;
    end

    assign  tx0_data_w_ecc = tx_data_w_ecc[0],
            tx1_data_w_ecc = tx_data_w_ecc[1],
            tx2_data_w_ecc = tx_data_w_ecc[2],
            tx3_data_w_ecc = tx_data_w_ecc[3],
            tx4_data_w_ecc = tx_data_w_ecc[4],
            tx5_data_w_ecc = tx_data_w_ecc[5],
            tx6_data_w_ecc = tx_data_w_ecc[6],
            tx7_data_w_ecc = tx_data_w_ecc[7];

    
endmodule : egr_tcu_epl_shim


