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
// -- Description  : PPE shared table memory wrapper
// ------------------------------------------------------------------- 

module ppe_stm_wrap
#(parameter BANKS               = 48,               //Number of banks
            CHUNKS              = 4,                //Number of chunks
            RDATA_WIDTH         = 72,               //Width of read data
            MEM_SIZE            = 4096,             //Number of entries in each memory
            RPORTS              = 6,                //Number of read ports
            ADDR_WIDTH          = 20,               //Width of address in a RAM
            WDATA_WIDTH         = 72,               //Width of write data
            LOG2_MEM_SIZE       = 12,               //Width of address bus output
            CHUNK_ADDR_WIDTH    = $clog2(CHUNKS-1), //Number of chunk address bits
            BANK_ADDR_WIDTH     = $clog2(BANKS-1)   //Number of bank address bits
) (
    input   logic                                                   cclk,

    input   logic   [RPORTS-1:0] [CHUNKS-1:0]                       i_ren,
    input   logic   [RPORTS-1:0] [ADDR_WIDTH-1:0]                   i_raddr,
    input   logic   [BANKS-1:0] [CHUNKS-1:0] [RDATA_WIDTH-1:0]      i_rdata,
    input   logic   [CHUNKS-1:0]                                    i_wen,
    input   logic   [ADDR_WIDTH-1:0]                                i_waddr,
    input   logic   [CHUNKS-1:0] [WDATA_WIDTH-1:0]                  i_wdata,

    output  logic   [BANKS-1:0] [CHUNKS-1:0]                        o_q_ren,
    output  logic   [BANKS-1:0] [CHUNKS-1:0]                        o_q_wen,
    output  logic   [BANKS-1:0] [CHUNKS-1:0] [LOG2_MEM_SIZE-1:0]    o_addr,
    output  logic   [RPORTS-1:0]                                    o_q_rvalid,
    output  logic   [RPORTS-1:0] [CHUNK_ADDR_WIDTH-1:0]             o_q_raddr,
    output  logic   [RPORTS-1:0] [CHUNKS-1:0] [RDATA_WIDTH-1:0]     o_q_rdata,
    output  logic   [CHUNKS-1:0] [WDATA_WIDTH-1:0]                  o_q_wdata
);

//TODO: Add write port
//TODO: Add collision handling
//TODO: Add clock gating
//TODO: Fix read data outputs to ensure proper alignment

logic   [BANKS-1:0] [CHUNKS-1:0] [$clog2(RPORTS)-1:0]               rd_sel;             //Per bank, per chunk port read select
logic   [BANKS-1:0] [CHUNKS-1:0] [$clog2(RPORTS)-1:0]               q_rd_sel_sg0;       //Flopped per bank port select
logic   [BANKS-1:0] [CHUNKS-1:0] [$clog2(RPORTS)-1:0]               q_rd_sel_sg1;       //Flopped per bank port select
logic   [BANKS-1:0] [CHUNKS-1:0]                                    rd_sel_valid;       //Per bank, per chunk port read select is valid
logic   [BANKS-1:0] [CHUNKS-1:0]                                    q_rd_sel_valid_sg0; //Per bank, per chunk port read select is valid
logic   [BANKS-1:0] [CHUNKS-1:0]                                    q_rd_sel_valid_sg1; //Per bank, per chunk port read select is valid
logic   [RPORTS-1:0] [CHUNKS-1:0] [ADDR_WIDTH-1:CHUNK_ADDR_WIDTH]   raddr;              //Per port, per chunk read address
logic   [RPORTS-1:0] [CHUNKS-1:0] [$clog2(MEM_SIZE)-1:0]            q_raddr;            //Flopped per port, per chunk read address
logic   [RPORTS-1:0] [CHUNKS-1:0]                                   q_ren_sg0;          //Flopped read enables per port, per chunk
logic   [RPORTS-1:0] [CHUNKS-1:0]                                   q_ren_sg1;          //Flopped read enables per port, per chunk
logic   [RPORTS-1:0]                                                q_ren_sg2;          //Flopped read enables per port, per chunk
logic   [BANKS-1:1] [RPORTS-1:0] [CHUNKS-1:0]                       q_port_ren;         //Flopped read enables, one per bank, per port, per chunk
logic   [BANKS-1:0] [RPORTS-1:0] [CHUNKS-1:0] [RDATA_WIDTH-1:0]     port_rdata;         //Per bank, per port per chunk read data
logic   [CHUNKS-1:0] [ADDR_WIDTH-1:CHUNK_ADDR_WIDTH]                waddr;              //Per chunk write address
logic   [CHUNKS-1:0] [$clog2(MEM_SIZE)-1:0]                         q_waddr;            //Flopped per chunk write address
logic   [RPORTS-1:0] [CHUNK_ADDR_WIDTH-1:0]                         q_raddr_sg0;        //Per port flopped chunk read address bits
logic   [RPORTS-1:0] [CHUNK_ADDR_WIDTH-1:0]                         q_raddr_sg1;        //Per port flopped chunk read address bits
logic   [RPORTS-1:0] [CHUNK_ADDR_WIDTH-1:0]                         q_raddr_sg2;        //Per port flopped chunk read address bits

//Create per-port, per-chunk read addresses
generate //{
    for(genvar g_i=0; g_i<RPORTS; g_i++) begin //{
        always_comb begin //{
            raddr[g_i][CHUNKS-1] = i_raddr[g_i][ADDR_WIDTH-1:CHUNK_ADDR_WIDTH];
            for(int i=0; i<CHUNKS-1; i++) begin //{
                if(i_raddr[g_i][CHUNK_ADDR_WIDTH-1:0] > i) raddr[g_i][i] = i_raddr[g_i][ADDR_WIDTH-1:CHUNK_ADDR_WIDTH] + 'd1;
                else raddr[g_i][i] = i_raddr[g_i][ADDR_WIDTH-1:CHUNK_ADDR_WIDTH];
            end //}
        end //}
        for(genvar g_j=0; g_j<CHUNKS; g_j++) begin //{
          always_ff @(posedge cclk) q_raddr[g_i][g_j] <= raddr[g_i][g_j][(ADDR_WIDTH-BANK_ADDR_WIDTH)-1:CHUNK_ADDR_WIDTH];
        end //}
    end //}
endgenerate //}

//Create per-chunk write addresses
always_comb begin //{
    waddr[CHUNKS-1] = i_waddr[ADDR_WIDTH-1:CHUNK_ADDR_WIDTH];
    for(int i=0; i<CHUNKS-1; i++) begin //{
        if(i_waddr[CHUNK_ADDR_WIDTH-1:0] > i) waddr[i] = i_waddr[ADDR_WIDTH-1:CHUNK_ADDR_WIDTH] + 'd1;
        else waddr[i] = i_waddr[ADDR_WIDTH-1:CHUNK_ADDR_WIDTH];
    end //}
end //}

generate //{
    for(genvar g_i=0; g_i<CHUNKS; g_i++) always_ff @(posedge cclk) q_waddr[g_i] <= waddr[g_i][(ADDR_WIDTH-BANK_ADDR_WIDTH)-1:CHUNK_ADDR_WIDTH];
endgenerate //}

//Create per bank, per chunk port read selects
generate //{
    for(genvar g_i=0; g_i<BANKS; g_i++) begin //{
        for(genvar g_j=0; g_j<CHUNKS; g_j++) begin //{
            always_comb begin //{
                rd_sel[g_i][g_j] = 'd0;
                rd_sel_valid[g_i][g_j] = 1'b0;
                for(int i=0; i<RPORTS; i++) begin //{
                    if(i_ren[i][g_j] && (raddr[i][g_j][ADDR_WIDTH-1:ADDR_WIDTH-BANK_ADDR_WIDTH] == g_i)) begin //{
                        rd_sel[g_i][g_j] = i;
                        rd_sel_valid[g_i][g_j] = 1'b1;
                    end //}
                end //}
            end //}
            always_ff @(posedge cclk) begin //{
                q_rd_sel_sg0[g_i][g_j] <= rd_sel[g_i][g_j];
                q_rd_sel_sg1[g_i][g_j] <= q_rd_sel_sg0[g_i][g_j];
                q_rd_sel_valid_sg0[g_i][g_j] <= rd_sel_valid[g_i][g_j];
                q_rd_sel_valid_sg1[g_i][g_j] <= q_rd_sel_valid_sg0[g_i][g_j];
            end //}
        end //}
    end //}
endgenerate //}

//Create per-bank, per-chunk read addresses
generate //{
    for(genvar g_i=0; g_i<BANKS; g_i++) begin //{
        for(genvar g_j=0; g_j<CHUNKS; g_j++) begin //{
            assign o_addr[g_i][g_j] = o_q_wen[g_i][g_j] ? q_waddr[g_j] : q_raddr[q_rd_sel_sg0[g_i][g_j]][g_j];
        end //}
    end //}
endgenerate //}

//Create per bank, per chunk read and write enables
generate //{
    for(genvar g_i=0; g_i<BANKS; g_i++) begin //{
        for(genvar g_j=0; g_j<CHUNKS; g_j++) begin //{
            always_ff @(posedge cclk) begin //{
                o_q_ren[g_i][g_j] <= rd_sel_valid[g_i][g_j] & i_ren[rd_sel[g_i][g_j]][g_j];
                o_q_wen[g_i][g_j] <= i_wen[g_j] & (waddr[g_j][ADDR_WIDTH-1:ADDR_WIDTH-BANK_ADDR_WIDTH] == g_i);
            end //}
        end //}
    end //}
endgenerate //}

//Flop write data
generate //{
    for(genvar g_i=0; g_i<CHUNKS; g_i++) always_ff @(posedge cclk) o_q_wdata[g_i] <= i_wdata[g_i];
endgenerate

//Read data muxing
generate //{
    for(genvar g_j=0; g_j<RPORTS; g_j++) begin //{
        always_ff @(posedge cclk) begin //{
            q_ren_sg0[g_j]      <= i_ren[g_j];
            q_ren_sg1[g_j]      <= q_ren_sg0[g_j];
            q_ren_sg2[g_j]  <= |q_ren_sg1[g_j];
            o_q_rvalid[g_j] <= q_ren_sg2[g_j];
            q_raddr_sg0[g_j]    <= i_raddr[g_j][CHUNK_ADDR_WIDTH-1:0];
            q_raddr_sg1[g_j]    <= q_raddr_sg0[g_j];
            q_raddr_sg2[g_j]    <= q_raddr_sg1[g_j];
            o_q_raddr[g_j]      <= q_raddr_sg2[g_j];
        end //}
        for(genvar g_k=0; g_k<CHUNKS; g_k++) begin //{
            always_comb port_rdata[0][g_j][g_k] = i_rdata[0][g_k];
            for(genvar g_i=1; g_i<BANKS; g_i++) begin //{
                always_ff @(posedge cclk) q_port_ren[g_i][g_j][g_k] <= q_rd_sel_valid_sg1[g_i][g_k] & q_ren_sg1[q_rd_sel_sg1[g_i][g_k]][g_k] & (q_rd_sel_sg1[g_i][g_k] == g_j);
                always_comb port_rdata[g_i][g_j][g_k] = q_port_ren[g_i][g_j][g_k] ? i_rdata[g_i][g_k] : port_rdata[g_i-1][g_j][g_k];
            end //}
        end //}
    end //}
endgenerate //}

//Assign read data outputs
generate //{
    for(genvar g_i=0; g_i<RPORTS; g_i++)
        for(genvar g_j=0; g_j<CHUNKS; g_j++)
            always_ff @(posedge cclk) o_q_rdata[g_i][g_j] <= port_rdata[BANKS-1][g_i][g_j];
endgenerate //}

endmodule // ppe_stm_wrap
