///------------------------------------------------------------------------------
///                                                                              
///  INTEL CONFIDENTIAL                                                          
///                                                                              
///  Copyright 2018 Intel Corporation All Rights Reserved.                 
///                                                                              
///  The source code contained or described herein and all documents related     
///  to the source code ("Material") are owned by Intel Corporation or its    
///  suppliers or licensors. Title to the Material remains with Intel            
///  Corporation or its suppliers and licensors. The Material contains trade     
///  secrets and proprietary and confidential information of Intel or its        
///  suppliers and licensors. The Material is protected by worldwide copyright   
///  and trade secret laws and treaty provisions. No part of the Material may    
///  be used, copied, reproduced, modified, published, uploaded, posted,         
///  transmitted, distributed, or disclosed in any way without Intel's prior     
///  express written permission.                                                 
///                                                                              
///  No license under any patent, copyright, trade secret or other intellectual  
///  property right is granted to or conferred upon you by disclosure or         
///  delivery of the Materials, either expressly, by implication, inducement,    
///  estoppel or otherwise. Any license under such intellectual property rights  
///  must be express and approved by Intel in writing.                           
///                                                                              
///------------------------------------------------------------------------------
// -- Author       : Jon Bagge <jon.bagge.com> 
// -- Project Name : MBY 
// -- Description  : PPE shared table memory logic top
// ------------------------------------------------------------------- 

module ppe_stm_logic
import axi2ibus_pkg::*;
#(parameter FWD_TBL0_BANKS          = 48,   //Number of banks in forwarding table 0
            FWD_TBL0_CHUNKS         = 4,    //Number of chunks in forwarding table 0
            FWD_TBL0_RDATA_WIDTH    = 72,   //Width of read data for forwarding table 0
            FWD_TBL0_MEM_SIZE       = 4096, //Number of entries in each memory in forwarding table 0
            LOG2_FWD_TBL0_MEM_SIZE  = 12,   //Width of forwarding table 0 address bus output
            FWD_TBL0_RPORTS         = 6,    //Number of read ports in forwarding table 0
            FWD_TBL0_ADDR_WIDTH     = 20,   //Width of address in a forwarding table 0 RAM
            FWD_TBL0_WDATA_WIDTH    = 72,   //Width of write data for forwarding table 0
            FWD_TBL1_BANKS          = 16,   //Number of banks in forwarding table 1
            FWD_TBL1_CHUNKS         = 4,    //Number of chunks in forwarding table 1
            FWD_TBL1_RDATA_WIDTH    = 72,   //Width of read data for forwarding table 1
            FWD_TBL1_MEM_SIZE       = 2047, //Number of entries in each memory in forwarding table 1
            LOG2_FWD_TBL1_MEM_SIZE  = 11,   //Width of forwarding table 1 address bus output
            FWD_TBL1_RPORTS         = 4,    //Number of read ports in forwarding table 1
            FWD_TBL1_ADDR_WIDTH     = 17,   //Width of address in a forwarding table 1 RAM
            FWD_TBL1_WDATA_WIDTH    = 72    //Width of write data for forwarding table 1
)
(
    input   logic                                                                           cclk,

    input   logic   [FWD_TBL0_BANKS-1:0] [FWD_TBL0_CHUNKS-1:0] [FWD_TBL0_RDATA_WIDTH-1:0]   i_fwd_tbl0_rdata,

    output  logic   [FWD_TBL0_BANKS-1:0] [FWD_TBL0_CHUNKS-1:0]                              o_q_fwd_tbl0_ren,
    output  logic   [FWD_TBL0_BANKS-1:0] [FWD_TBL0_CHUNKS-1:0]                              o_q_fwd_tbl0_wen,
    output  logic   [FWD_TBL0_BANKS-1:0] [FWD_TBL0_CHUNKS-1:0] [LOG2_FWD_TBL0_MEM_SIZE-1:0] o_fwd_tbl0_addr,
    output  logic   [FWD_TBL0_CHUNKS-1:0] [FWD_TBL0_WDATA_WIDTH-1:0]                        o_q_fwd_tbl0_wdata,

    input   logic   [FWD_TBL1_BANKS-1:0] [FWD_TBL1_CHUNKS-1:0] [FWD_TBL1_RDATA_WIDTH-1:0]   i_fwd_tbl1_rdata,

    output  logic   [FWD_TBL1_BANKS-1:0] [FWD_TBL1_CHUNKS-1:0]                              o_q_fwd_tbl1_ren,
    output  logic   [FWD_TBL1_BANKS-1:0] [FWD_TBL1_CHUNKS-1:0]                              o_q_fwd_tbl1_wen,
    output  logic   [FWD_TBL1_BANKS-1:0] [FWD_TBL1_CHUNKS-1:0] [LOG2_FWD_TBL1_MEM_SIZE-1:0] o_fwd_tbl1_addr,
    output  logic   [FWD_TBL1_CHUNKS-1:0] [FWD_TBL1_WDATA_WIDTH-1:0]                        o_q_fwd_tbl1_wdata,

    input   ibus_ctrl_t                                                                     i_ibus_ctrl,
    output  ibus_resp_t                                                                     o_ibus_resp,

    rx_ppe_ppe_stm0_if.stm                                                                  rx_ppe_ppe_stm0_if0,
    rx_ppe_ppe_stm1_if.stm                                                                  rx_ppe_ppe_stm1_if0,
    rx_ppe_ppe_stm0_if.stm                                                                  rx_ppe_ppe_stm0_if1,
    rx_ppe_ppe_stm1_if.stm                                                                  rx_ppe_ppe_stm1_if1,

    egr_ppe_stm_if.stm                                                                      egr_ppe_stm_if0,
    egr_ppe_stm_if.stm                                                                      egr_ppe_stm_if1,

    egr_mc_table_if.mc_table                                                                mc_table_if0_0,
    egr_mc_table_if.mc_table                                                                mc_table_if0_1,
    egr_mc_table_if.mc_table                                                                mc_table_if1_0,
    egr_mc_table_if.mc_table                                                                mc_table_if1_1
);

//TODO: Add write port driving
//TODO: Add collision handling
//TODO: Add clock gating

logic   [FWD_TBL0_RPORTS-1:0] [FWD_TBL0_CHUNKS-1:0]                             q_fwd_tbl0_ren;     //Per port, per chunk forwarding table 0 read enable
logic   [FWD_TBL0_RPORTS-1:0] [FWD_TBL0_ADDR_WIDTH-1:0]                         q_fwd_tbl0_raddr;   //Per port, forwarding table 0 read address
logic   [FWD_TBL0_RPORTS-1:0]                                                   fwd_tbl0_rvalid;    //Per port forwarding table 0 read valid
logic   [FWD_TBL0_RPORTS-1:0] [1:0]                                             fwd_tbl0_raddr;     //Per port, forwarding table 0 read address
logic   [FWD_TBL0_RPORTS-1:0] [FWD_TBL0_CHUNKS-1:0] [FWD_TBL0_RDATA_WIDTH-1:0]  fwd_tbl0_rdata;     //Per port per chunk forwarding table 0 read data

logic   [FWD_TBL1_RPORTS-1:0] [FWD_TBL1_CHUNKS-1:0]                             q_fwd_tbl1_ren;     //Per port, per chunk forwarding table 1 read enable
logic   [FWD_TBL1_RPORTS-1:0] [FWD_TBL1_ADDR_WIDTH-1:0]                         q_fwd_tbl1_raddr;   //Per port, forwarding table 1 read address
logic   [FWD_TBL1_RPORTS-1:0]                                                   fwd_tbl1_rvalid;    //Per port forwarding table 1 read valid
logic   [FWD_TBL1_RPORTS-1:0] [1:0]                                             fwd_tbl1_raddr;     //Per port, forwarding table 1 read address
logic   [FWD_TBL1_RPORTS-1:0] [FWD_TBL1_CHUNKS-1:0] [FWD_TBL1_RDATA_WIDTH-1:0]  fwd_tbl1_rdata;     //Per port per chunk forwarding table 1 read data

logic   [FWD_TBL0_CHUNKS-1:0]                               q_fwd_tbl0_wen;     //Per chunk forwarding table 0 write enable
logic   [FWD_TBL0_ADDR_WIDTH-1:0]                           q_fwd_tbl0_waddr;   //Forwarding table 0 write address
logic   [FWD_TBL0_CHUNKS-1:0] [FWD_TBL0_WDATA_WIDTH-1:0]    q_fwd_tbl0_wdata;   //Per chunk forwarding table 0 write data

logic   [FWD_TBL1_CHUNKS-1:0]                               q_fwd_tbl1_wen;     //Per chunk forwarding table 1 write enable
logic   [FWD_TBL1_ADDR_WIDTH-1:0]                           q_fwd_tbl1_waddr;   //Forwarding table 1 write address
logic   [FWD_TBL1_CHUNKS-1:0] [FWD_TBL1_WDATA_WIDTH-1:0]    q_fwd_tbl1_wdata;   //Per chunk forwarding table 1 write data

always_ff @(posedge cclk) begin //{
    q_fwd_tbl0_ren[(FWD_TBL0_RPORTS/2)-1:0]     <= rx_ppe_ppe_stm0_if0.tbl_ren;
    q_fwd_tbl0_raddr[(FWD_TBL0_RPORTS/2)-1:0]   <= rx_ppe_ppe_stm0_if0.tbl_raddr;
    rx_ppe_ppe_stm0_if0.tbl_lpm_rvalid          <= fwd_tbl0_rvalid[0];
    rx_ppe_ppe_stm0_if0.tbl_em_rvalid           <= fwd_tbl0_rvalid[2:1];
    if(fwd_tbl0_rvalid[0]) begin //{
        case(fwd_tbl0_raddr[0]) //{
            2'h0: rx_ppe_ppe_stm0_if0.tbl_lpm_rdata     <= {fwd_tbl0_rdata[0][1],fwd_tbl0_rdata[0][0]};
            2'h1: rx_ppe_ppe_stm0_if0.tbl_lpm_rdata     <= {fwd_tbl0_rdata[0][2],fwd_tbl0_rdata[0][1]};
            2'h2: rx_ppe_ppe_stm0_if0.tbl_lpm_rdata     <= {fwd_tbl0_rdata[0][3],fwd_tbl0_rdata[0][2]};
            default: rx_ppe_ppe_stm0_if0.tbl_lpm_rdata  <= {fwd_tbl0_rdata[0][0],fwd_tbl0_rdata[0][3]};
        endcase //}
    end //}
    if(fwd_tbl0_rvalid[1]) begin //{
        case(fwd_tbl0_raddr[1]) //{
            2'h0: rx_ppe_ppe_stm0_if0.tbl_em_rdata[0]       <= {fwd_tbl0_rdata[1][3],fwd_tbl0_rdata[1][2],fwd_tbl0_rdata[1][1],fwd_tbl0_rdata[1][0]};
            2'h1: rx_ppe_ppe_stm0_if0.tbl_em_rdata[0]       <= {fwd_tbl0_rdata[1][0],fwd_tbl0_rdata[1][3],fwd_tbl0_rdata[1][2],fwd_tbl0_rdata[1][1]};
            2'h2: rx_ppe_ppe_stm0_if0.tbl_em_rdata[0]       <= {fwd_tbl0_rdata[1][1],fwd_tbl0_rdata[1][0],fwd_tbl0_rdata[1][3],fwd_tbl0_rdata[1][2]};
            default: rx_ppe_ppe_stm0_if0.tbl_em_rdata[0]    <= {fwd_tbl0_rdata[1][2],fwd_tbl0_rdata[1][1],fwd_tbl0_rdata[1][0],fwd_tbl0_rdata[1][3]};
        endcase //}
    end //}
    if(fwd_tbl0_rvalid[2]) begin //{
        case(fwd_tbl0_raddr[2]) //{
            2'h0: rx_ppe_ppe_stm0_if0.tbl_em_rdata[1]       <= {fwd_tbl0_rdata[2][3],fwd_tbl0_rdata[2][2],fwd_tbl0_rdata[2][1],fwd_tbl0_rdata[2][0]};
            2'h1: rx_ppe_ppe_stm0_if0.tbl_em_rdata[1]       <= {fwd_tbl0_rdata[2][0],fwd_tbl0_rdata[2][3],fwd_tbl0_rdata[2][2],fwd_tbl0_rdata[2][1]};
            2'h2: rx_ppe_ppe_stm0_if0.tbl_em_rdata[1]       <= {fwd_tbl0_rdata[2][1],fwd_tbl0_rdata[2][0],fwd_tbl0_rdata[2][3],fwd_tbl0_rdata[2][2]};
            default: rx_ppe_ppe_stm0_if0.tbl_em_rdata[1]    <= {fwd_tbl0_rdata[2][2],fwd_tbl0_rdata[2][1],fwd_tbl0_rdata[2][0],fwd_tbl0_rdata[2][3]};
        endcase //}
    end //}

    q_fwd_tbl0_ren[FWD_TBL0_RPORTS-1:(FWD_TBL0_RPORTS/2)]   <= rx_ppe_ppe_stm0_if1.tbl_ren;
    q_fwd_tbl0_raddr[FWD_TBL0_RPORTS-1:(FWD_TBL0_RPORTS/2)] <= rx_ppe_ppe_stm0_if1.tbl_raddr;
    rx_ppe_ppe_stm0_if1.tbl_lpm_rvalid                      <= fwd_tbl0_rvalid[3];
    rx_ppe_ppe_stm0_if1.tbl_em_rvalid                       <= fwd_tbl0_rvalid[5:4];
    if(fwd_tbl0_rvalid[3]) begin //{
        case(fwd_tbl0_raddr[3]) //{
            2'h0: rx_ppe_ppe_stm0_if1.tbl_lpm_rdata     <= {fwd_tbl0_rdata[3][1],fwd_tbl0_rdata[3][0]};
            2'h1: rx_ppe_ppe_stm0_if1.tbl_lpm_rdata     <= {fwd_tbl0_rdata[3][2],fwd_tbl0_rdata[3][1]};
            2'h2: rx_ppe_ppe_stm0_if1.tbl_lpm_rdata     <= {fwd_tbl0_rdata[3][3],fwd_tbl0_rdata[3][2]};
            default: rx_ppe_ppe_stm0_if1.tbl_lpm_rdata  <= {fwd_tbl0_rdata[3][0],fwd_tbl0_rdata[3][3]};
        endcase //}
    end //}
    if(fwd_tbl0_rvalid[4]) begin //{
        case(fwd_tbl0_raddr[4]) //{
            2'h0: rx_ppe_ppe_stm0_if1.tbl_em_rdata[0]       <= {fwd_tbl0_rdata[4][3],fwd_tbl0_rdata[4][2],fwd_tbl0_rdata[4][1],fwd_tbl0_rdata[4][0]};
            2'h1: rx_ppe_ppe_stm0_if1.tbl_em_rdata[0]       <= {fwd_tbl0_rdata[4][0],fwd_tbl0_rdata[4][3],fwd_tbl0_rdata[4][2],fwd_tbl0_rdata[4][1]};
            2'h2: rx_ppe_ppe_stm0_if1.tbl_em_rdata[0]       <= {fwd_tbl0_rdata[4][1],fwd_tbl0_rdata[4][0],fwd_tbl0_rdata[4][3],fwd_tbl0_rdata[4][2]};
            default: rx_ppe_ppe_stm0_if1.tbl_em_rdata[0]    <= {fwd_tbl0_rdata[4][2],fwd_tbl0_rdata[4][1],fwd_tbl0_rdata[4][0],fwd_tbl0_rdata[4][3]};
        endcase //}
    end //}
    if(fwd_tbl0_rvalid[5]) begin //{
        case(fwd_tbl0_raddr[5]) //{
            2'h0: rx_ppe_ppe_stm0_if1.tbl_em_rdata[1]       <= {fwd_tbl0_rdata[5][3],fwd_tbl0_rdata[5][2],fwd_tbl0_rdata[5][1],fwd_tbl0_rdata[5][0]};
            2'h1: rx_ppe_ppe_stm0_if1.tbl_em_rdata[1]       <= {fwd_tbl0_rdata[5][0],fwd_tbl0_rdata[5][3],fwd_tbl0_rdata[5][2],fwd_tbl0_rdata[5][1]};
            2'h2: rx_ppe_ppe_stm0_if1.tbl_em_rdata[1]       <= {fwd_tbl0_rdata[5][1],fwd_tbl0_rdata[5][0],fwd_tbl0_rdata[5][3],fwd_tbl0_rdata[5][2]};
            default: rx_ppe_ppe_stm0_if1.tbl_em_rdata[1]    <= {fwd_tbl0_rdata[5][2],fwd_tbl0_rdata[5][1],fwd_tbl0_rdata[5][0],fwd_tbl0_rdata[5][3]};
        endcase //}
    end //}

    q_fwd_tbl1_ren[(FWD_TBL1_RPORTS/2)-1:0]     <= rx_ppe_ppe_stm1_if0.tbl_ren;
    q_fwd_tbl1_raddr[(FWD_TBL1_RPORTS/2)-1:0]   <= rx_ppe_ppe_stm1_if0.tbl_raddr;
    rx_ppe_ppe_stm1_if0.tbl_em_rvalid           <= fwd_tbl1_rvalid[1:0];
    if(fwd_tbl1_rvalid[0]) begin //{
        case(fwd_tbl1_raddr[0]) //{
            2'h0: rx_ppe_ppe_stm1_if0.tbl_em_rdata[0]       <= {fwd_tbl1_rdata[0][3],fwd_tbl1_rdata[0][2],fwd_tbl1_rdata[0][1],fwd_tbl1_rdata[0][0]};
            2'h1: rx_ppe_ppe_stm1_if0.tbl_em_rdata[0]       <= {fwd_tbl1_rdata[0][0],fwd_tbl1_rdata[0][3],fwd_tbl1_rdata[0][2],fwd_tbl1_rdata[0][1]};
            2'h2: rx_ppe_ppe_stm1_if0.tbl_em_rdata[0]       <= {fwd_tbl1_rdata[0][1],fwd_tbl1_rdata[0][0],fwd_tbl1_rdata[0][3],fwd_tbl1_rdata[0][2]};
            default: rx_ppe_ppe_stm1_if0.tbl_em_rdata[0]    <= {fwd_tbl1_rdata[0][2],fwd_tbl1_rdata[0][1],fwd_tbl1_rdata[0][0],fwd_tbl1_rdata[0][3]};
        endcase //}
    end //}
    if(fwd_tbl1_rvalid[1]) begin //{
        case(fwd_tbl1_raddr[1]) //{
            2'h0: rx_ppe_ppe_stm1_if0.tbl_em_rdata[1]       <= {fwd_tbl1_rdata[1][3],fwd_tbl1_rdata[1][2],fwd_tbl1_rdata[1][1],fwd_tbl1_rdata[1][0]};
            2'h1: rx_ppe_ppe_stm1_if0.tbl_em_rdata[1]       <= {fwd_tbl1_rdata[1][0],fwd_tbl1_rdata[1][3],fwd_tbl1_rdata[1][2],fwd_tbl1_rdata[1][1]};
            2'h2: rx_ppe_ppe_stm1_if0.tbl_em_rdata[1]       <= {fwd_tbl1_rdata[1][1],fwd_tbl1_rdata[1][0],fwd_tbl1_rdata[1][3],fwd_tbl1_rdata[1][2]};
            default: rx_ppe_ppe_stm1_if0.tbl_em_rdata[1]    <= {fwd_tbl1_rdata[1][2],fwd_tbl1_rdata[1][1],fwd_tbl1_rdata[1][0],fwd_tbl1_rdata[1][3]};
        endcase //}
    end //}

    q_fwd_tbl1_ren[FWD_TBL1_RPORTS-1:(FWD_TBL1_RPORTS/2)]   <= rx_ppe_ppe_stm1_if1.tbl_ren;
    q_fwd_tbl1_raddr[FWD_TBL1_RPORTS-1:(FWD_TBL1_RPORTS/2)] <= rx_ppe_ppe_stm1_if1.tbl_raddr;
    rx_ppe_ppe_stm1_if1.tbl_em_rvalid                       <= fwd_tbl1_rvalid[3:2];
    if(fwd_tbl0_rvalid[2]) begin //{
        case(fwd_tbl1_raddr[2]) //{
            2'h0: rx_ppe_ppe_stm1_if1.tbl_em_rdata[0]       <= {fwd_tbl1_rdata[2][3],fwd_tbl1_rdata[2][2],fwd_tbl1_rdata[2][1],fwd_tbl1_rdata[2][0]};
            2'h1: rx_ppe_ppe_stm1_if1.tbl_em_rdata[0]       <= {fwd_tbl1_rdata[2][0],fwd_tbl1_rdata[2][3],fwd_tbl1_rdata[2][2],fwd_tbl1_rdata[2][1]};
            2'h2: rx_ppe_ppe_stm1_if1.tbl_em_rdata[0]       <= {fwd_tbl1_rdata[2][1],fwd_tbl1_rdata[2][0],fwd_tbl1_rdata[2][3],fwd_tbl1_rdata[2][2]};
            default: rx_ppe_ppe_stm1_if1.tbl_em_rdata[0]    <= {fwd_tbl1_rdata[2][2],fwd_tbl1_rdata[2][1],fwd_tbl1_rdata[2][0],fwd_tbl1_rdata[2][3]};
        endcase //}
    end //}
    if(fwd_tbl1_rvalid[3]) begin //{
        case(fwd_tbl1_raddr[3]) //{
            2'h0: rx_ppe_ppe_stm1_if1.tbl_em_rdata[1]       <= {fwd_tbl1_rdata[3][3],fwd_tbl1_rdata[3][2],fwd_tbl1_rdata[3][1],fwd_tbl1_rdata[3][0]};
            2'h1: rx_ppe_ppe_stm1_if1.tbl_em_rdata[1]       <= {fwd_tbl1_rdata[3][0],fwd_tbl1_rdata[3][3],fwd_tbl1_rdata[3][2],fwd_tbl1_rdata[3][1]};
            2'h2: rx_ppe_ppe_stm1_if1.tbl_em_rdata[1]       <= {fwd_tbl1_rdata[3][1],fwd_tbl1_rdata[3][0],fwd_tbl1_rdata[3][3],fwd_tbl1_rdata[3][2]};
            default: rx_ppe_ppe_stm1_if1.tbl_em_rdata[1]    <= {fwd_tbl1_rdata[3][2],fwd_tbl1_rdata[3][1],fwd_tbl1_rdata[3][0],fwd_tbl1_rdata[3][3]};
        endcase //}
    end //}
end //}

ppe_stm_wrap #(
    .BANKS          (FWD_TBL0_BANKS),
    .CHUNKS         (FWD_TBL0_CHUNKS),
    .RDATA_WIDTH    (FWD_TBL0_RDATA_WIDTH),
    .MEM_SIZE       (FWD_TBL0_MEM_SIZE),
    .RPORTS         (FWD_TBL0_RPORTS),
    .ADDR_WIDTH     (FWD_TBL0_ADDR_WIDTH),
    .WDATA_WIDTH    (FWD_TBL0_WDATA_WIDTH),
    .LOG2_MEM_SIZE  (LOG2_FWD_TBL0_MEM_SIZE)
) shm_fwd_tbl0 (
    .cclk           (cclk),

    .i_ren      (q_fwd_tbl0_ren),
    .i_raddr    (q_fwd_tbl0_raddr),
    .i_rdata    (i_fwd_tbl0_rdata),
    .i_wen      (q_fwd_tbl0_wen),
    .i_waddr    (q_fwd_tbl0_waddr),
    .i_wdata    (q_fwd_tbl0_wdata),

    .o_q_ren    (o_q_fwd_tbl0_ren),
    .o_q_wen    (o_q_fwd_tbl0_wen),
    .o_addr     (o_fwd_tbl0_addr),
    .o_q_rvalid (fwd_tbl0_rvalid),
    .o_q_raddr  (fwd_tbl0_raddr),
    .o_q_rdata  (fwd_tbl0_rdata),
    .o_q_wdata  (o_q_fwd_tbl0_wdata)
);

ppe_stm_wrap #(
    .BANKS          (FWD_TBL1_BANKS),
    .CHUNKS         (FWD_TBL1_CHUNKS),
    .RDATA_WIDTH    (FWD_TBL1_RDATA_WIDTH),
    .MEM_SIZE       (FWD_TBL1_MEM_SIZE),
    .RPORTS         (FWD_TBL1_RPORTS),
    .ADDR_WIDTH     (FWD_TBL1_ADDR_WIDTH),
    .WDATA_WIDTH    (FWD_TBL1_WDATA_WIDTH),
    .LOG2_MEM_SIZE  (LOG2_FWD_TBL1_MEM_SIZE)
) shm_fwd_tbl1 (
    .cclk           (cclk),

    .i_ren      (q_fwd_tbl1_ren),
    .i_raddr    (q_fwd_tbl1_raddr),
    .i_rdata    (i_fwd_tbl1_rdata),
    .i_wen      (q_fwd_tbl1_wen),
    .i_waddr    (q_fwd_tbl1_waddr),
    .i_wdata    (q_fwd_tbl1_wdata),

    .o_q_ren    (o_q_fwd_tbl1_ren),
    .o_q_wen    (o_q_fwd_tbl1_wen),
    .o_addr     (o_fwd_tbl1_addr),
    .o_q_rvalid (fwd_tbl1_rvalid),
    .o_q_raddr  (fwd_tbl1_raddr),
    .o_q_rdata  (fwd_tbl1_rdata),
    .o_q_wdata  (o_q_fwd_tbl1_wdata)
);

endmodule // ppe_stm_logic
