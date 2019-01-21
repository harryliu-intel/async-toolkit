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
// ---------------------------------------------------------------------------------------------------------------------


`include "mby_msh_node_defines.vh";                                // include file with `defines 
`include "mby_mesh_row_map_pkg.vh"                                 // Nebulon generated CSR

module outside_node 

import mby_msh_pkg::*;                                             // import declarations from mby_msh_pkg.sv
import mby_msh_node_pkg::*;                                        // import declarations from mby_msh_node_pkg.sv
#(

    parameter int NUM_MSH_ROWS    = MAX_NUM_MSH_ROWS,               // number of mesh rows
    parameter int NUM_MSH_COLS    = MAX_NUM_MSH_COLS                // number of mesh columns

)
(
    input                   mclk,                                 // mesh clock                                 
    input                   mhreset,                              // mesh hard reset
    input                   msreset,                              // mesh soft reset

    input msh_col_wr_req_t i_nb_wr_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing northbound wr request 
    input msh_col_wr_req_t i_sb_wr_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing southbound wr request 
    input msh_row_wr_req_t i_eb_wr_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing eastbound wr request 
    input msh_row_wr_req_t i_wb_wr_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing westbound wr request 

    input msh_data_t       i_nb_wr_data [NUM_MSH_PLANES-1:0],    // receive from node outgoing northbound wr data bus 
    input msh_data_t       i_sb_wr_data [NUM_MSH_PLANES-1:0],    // receive from node outgoing southbound wr data bus 
    input msh_data_t       i_eb_wr_data [NUM_MSH_PLANES-1:0],    // receive from node outgoing eastbound wr data bus 
    input msh_data_t       i_wb_wr_data [NUM_MSH_PLANES-1:0],    // receive from node outgoing westbound wr data bus 

    input msh_col_rd_req_t i_nb_rd_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing northbound read request 
    input msh_col_rd_req_t i_sb_rd_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing southbound read request 
    input msh_row_rd_req_t i_eb_rd_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing eastbound read request 
    input msh_row_rd_req_t i_wb_rd_req  [NUM_MSH_PLANES-1:0],    // receive from node outgoing westbound read request 

    output  msh_col_rd_rsp_t o_nb_rd_rsp  [NUM_MSH_PLANES-1:0],    // go to node incoming northbound read response 
    output  msh_col_rd_rsp_t o_sb_rd_rsp  [NUM_MSH_PLANES-1:0],    // go to node incoming southbound read response 
    output  msh_row_rd_rsp_t o_eb_rd_rsp  [NUM_MSH_PLANES-1:0],    // go to node incoming eastbound read response 
    output  msh_row_rd_rsp_t o_wb_rd_rsp  [NUM_MSH_PLANES-1:0],    // go to node incoming westbound read response 

    output  msh_data_t       o_nb_rd_data [NUM_MSH_PLANES-1:0],    // go to node incoming northbound read data bus 
    output  msh_data_t       o_sb_rd_data [NUM_MSH_PLANES-1:0],    // go to node incoming southbound read data bus 
    output  msh_data_t       o_eb_rd_data [NUM_MSH_PLANES-1:0],    // go to node incoming eastbound read data bus 
    output  msh_data_t       o_wb_rd_data [NUM_MSH_PLANES-1:0]    // go to node incoming westbound read data bus 
);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------
  logic mhsreset;  
  assign mhsreset   =  mhreset | msreset;


// register eb_wr

msh_row_wr_req_t eb_wr_req_q1  [NUM_MSH_PLANES-1:0];
msh_row_wr_req_t eb_wr_req_q2  [NUM_MSH_PLANES-1:0];
msh_row_wr_req_t eb_wr_req_save_d  [NUM_MSH_PLANES-1:0];
msh_row_wr_req_t eb_wr_req_save  [NUM_MSH_PLANES-1:0];
msh_data_t       eb_wr_data_save_d [NUM_MSH_PLANES-1:0];
msh_data_t       eb_wr_data_save [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	eb_wr_req_q1[0]	<= '0;
	eb_wr_req_q2[0]	<= '0;
	eb_wr_req_q1[1]	<= '0;
	eb_wr_req_q2[1]	<= '0;
    end
    else begin
	eb_wr_req_q1[0]	<= i_eb_wr_req[0];
	eb_wr_req_q2[0]	<= eb_wr_req_q1[0];
	eb_wr_req_q1[1]	<= i_eb_wr_req[1];
	eb_wr_req_q2[1]	<= eb_wr_req_q1[1];
    end
end;

assign eb_wr_req_save_d[0] = (eb_wr_req_q2[0].vld) ? eb_wr_req_q2[0] : eb_wr_req_save[0];
assign eb_wr_req_save_d[1] = (eb_wr_req_q2[1].vld) ? eb_wr_req_q2[1] : eb_wr_req_save[1];

assign eb_wr_data_save_d[0] = (eb_wr_req_q2[0].vld) ? i_eb_wr_data[0] : eb_wr_data_save[0];
assign eb_wr_data_save_d[1] = (eb_wr_req_q2[1].vld) ? i_eb_wr_data[1] : eb_wr_data_save[1];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	eb_wr_req_save[0]	<= '0;
	eb_wr_req_save[1]	<= '0;
	eb_wr_data_save[0]	<= '0;
	eb_wr_data_save[1]	<= '0;
    end
    else begin
	eb_wr_req_save[0]	<= eb_wr_req_save_d[0];
	eb_wr_req_save[1]	<= eb_wr_req_save_d[1];
	eb_wr_data_save[0]	<= eb_wr_data_save_d[0];
	eb_wr_data_save[1]	<= eb_wr_data_save_d[1];
    end
end;


// register wb_wr

 msh_row_wr_req_t wb_wr_req_q1  [NUM_MSH_PLANES-1:0];
 msh_row_wr_req_t wb_wr_req_q2  [NUM_MSH_PLANES-1:0];
 msh_row_wr_req_t wb_wr_req_save_d  [NUM_MSH_PLANES-1:0];
 msh_row_wr_req_t wb_wr_req_save  [NUM_MSH_PLANES-1:0];
 msh_data_t       wb_wr_data_save_d [NUM_MSH_PLANES-1:0];
 msh_data_t       wb_wr_data_save [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	wb_wr_req_q1[0]	<= '0;
	wb_wr_req_q2[0]	<= '0;
	wb_wr_req_q1[1]	<= '0;
	wb_wr_req_q2[1]	<= '0;
    end
    else begin
	wb_wr_req_q1[0]	<= i_wb_wr_req[0];
	wb_wr_req_q2[0]	<= wb_wr_req_q1[0];
	wb_wr_req_q1[1]	<= i_wb_wr_req[1];
	wb_wr_req_q2[1]	<= wb_wr_req_q1[1];
    end
end;

assign wb_wr_req_save_d[0] = (wb_wr_req_q2[0].vld) ? wb_wr_req_q2[0] : wb_wr_req_save[0];
assign wb_wr_req_save_d[1] = (wb_wr_req_q2[1].vld) ? wb_wr_req_q2[1] : wb_wr_req_save[1];

assign wb_wr_data_save_d[0] = (wb_wr_req_q2[0].vld) ? i_wb_wr_data[0] : wb_wr_data_save[0];
assign wb_wr_data_save_d[1] = (wb_wr_req_q2[1].vld) ? i_wb_wr_data[1] : wb_wr_data_save[1];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	wb_wr_req_save[0]	<= '0;
	wb_wr_req_save[1]	<= '0;
	wb_wr_data_save[0]	<= '0;
	wb_wr_data_save[1]	<= '0;
    end
    else begin
	wb_wr_req_save[0]	<= wb_wr_req_save_d[0];
	wb_wr_req_save[1]	<= wb_wr_req_save_d[1];
	wb_wr_data_save[0]	<= wb_wr_data_save_d[0];
	wb_wr_data_save[1]	<= wb_wr_data_save_d[1];
    end
end;


// register nb_wr

 msh_col_wr_req_t  nb_wr_req_q1  [NUM_MSH_PLANES-1:0];
 msh_col_wr_req_t  nb_wr_req_q2  [NUM_MSH_PLANES-1:0];
 msh_col_wr_req_t  nb_wr_req_save_d  [NUM_MSH_PLANES-1:0];
 msh_col_wr_req_t  nb_wr_req_save  [NUM_MSH_PLANES-1:0];
 msh_data_t        nb_wr_data_save_d [NUM_MSH_PLANES-1:0];
 msh_data_t        nb_wr_data_save [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	nb_wr_req_q1[0]	<= '0;
	nb_wr_req_q2[0]	<= '0;
	nb_wr_req_q1[1]	<= '0;
	nb_wr_req_q2[1]	<= '0;
    end
    else begin
	nb_wr_req_q1[0]	<= i_nb_wr_req[0];
	nb_wr_req_q2[0]	<= nb_wr_req_q1[0];
	nb_wr_req_q1[1]	<= i_nb_wr_req[1];
	nb_wr_req_q2[1]	<= nb_wr_req_q1[1];
    end
end;

assign nb_wr_req_save_d[0] = (nb_wr_req_q2[0].vld) ? nb_wr_req_q2[0] : nb_wr_req_save[0];
assign nb_wr_req_save_d[1] = (nb_wr_req_q2[1].vld) ? nb_wr_req_q2[1] : nb_wr_req_save[1];

assign nb_wr_data_save_d[0] = (nb_wr_req_q2[0].vld) ? i_nb_wr_data[0] : nb_wr_data_save[0];
assign nb_wr_data_save_d[1] = (nb_wr_req_q2[1].vld) ? i_nb_wr_data[1] : nb_wr_data_save[1];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	nb_wr_req_save[0]	<= '0;
	nb_wr_req_save[1]	<= '0;
	nb_wr_data_save[0]	<= '0;
	nb_wr_data_save[1]	<= '0;
    end
    else begin
	nb_wr_req_save[0]	<= nb_wr_req_save_d[0];
	nb_wr_req_save[1]	<= nb_wr_req_save_d[1];
	nb_wr_data_save[0]	<= nb_wr_data_save_d[0];
	nb_wr_data_save[1]	<= nb_wr_data_save_d[1];
    end
end;


// register sb_wr

 msh_col_wr_req_t  sb_wr_req_q1  [NUM_MSH_PLANES-1:0];
 msh_col_wr_req_t  sb_wr_req_q2  [NUM_MSH_PLANES-1:0];
 msh_col_wr_req_t  sb_wr_req_save_d  [NUM_MSH_PLANES-1:0];
 msh_col_wr_req_t  sb_wr_req_save  [NUM_MSH_PLANES-1:0];
 msh_data_t        sb_wr_data_save_d [NUM_MSH_PLANES-1:0];
 msh_data_t        sb_wr_data_save [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	sb_wr_req_q1[0]	<= '0;
	sb_wr_req_q2[0]	<= '0;
	sb_wr_req_q1[1]	<= '0;
	sb_wr_req_q2[1]	<= '0;
    end
    else begin
	sb_wr_req_q1[0]	<= i_sb_wr_req[0];
	sb_wr_req_q2[0]	<= sb_wr_req_q1[0];
	sb_wr_req_q1[1]	<= i_sb_wr_req[1];
	sb_wr_req_q2[1]	<= sb_wr_req_q1[1];
    end
end;

assign sb_wr_req_save_d[0] = (sb_wr_req_q2[0].vld) ? sb_wr_req_q2[0] : sb_wr_req_save[0];
assign sb_wr_req_save_d[1] = (sb_wr_req_q2[1].vld) ? sb_wr_req_q2[1] : sb_wr_req_save[1];

assign sb_wr_data_save_d[0] = (sb_wr_req_q2[0].vld) ? i_sb_wr_data[0] : sb_wr_data_save[0];
assign sb_wr_data_save_d[1] = (sb_wr_req_q2[1].vld) ? i_sb_wr_data[1] : sb_wr_data_save[1];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	sb_wr_req_save[0]	<= '0;
	sb_wr_req_save[1]	<= '0;
	sb_wr_data_save[0]	<= '0;
	sb_wr_data_save[1]	<= '0;
    end
    else begin
	sb_wr_req_save[0]	<= sb_wr_req_save_d[0];
	sb_wr_req_save[1]	<= sb_wr_req_save_d[1];
	sb_wr_data_save[0]	<= sb_wr_data_save_d[0];
	sb_wr_data_save[1]	<= sb_wr_data_save_d[1];
    end
end;



// register eb_rd_req

msh_row_rd_req_t eb_rd_req_q1  [NUM_MSH_PLANES-1:0];
msh_row_rd_req_t eb_rd_req_q2  [NUM_MSH_PLANES-1:0];
msh_row_rd_req_t eb_rd_req_q3  [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	eb_rd_req_q1[0]	<= '0;
	eb_rd_req_q2[0]	<= '0;
	eb_rd_req_q3[0]	<= '0;
	eb_rd_req_q1[1]	<= '0;
	eb_rd_req_q2[1]	<= '0;
	eb_rd_req_q3[1]	<= '0;
    end
    else begin
	eb_rd_req_q1[0]	<= i_eb_rd_req[0];
	eb_rd_req_q2[0]	<= eb_rd_req_q1[0];
	eb_rd_req_q3[0]	<= eb_rd_req_q2[0];
	eb_rd_req_q1[1]	<= i_eb_rd_req[1];
	eb_rd_req_q2[1]	<= eb_rd_req_q1[1];
	eb_rd_req_q3[1]	<= eb_rd_req_q2[1];
    end
end;


msh_row_rd_rsp_t wb_rd_rsp [NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t wb_rd_rsp_q1 [NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t wb_rd_rsp_q2 [NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t wb_rd_rsp_q3 [NUM_MSH_PLANES-1:0];

assign wb_rd_rsp[0].vld = eb_rd_req_q3[0].vld;
assign wb_rd_rsp[0].id = eb_rd_req_q3[0].id;

assign wb_rd_rsp[1].vld = eb_rd_req_q3[1].vld;
assign wb_rd_rsp[1].id = eb_rd_req_q3[1].id;

always_ff @(posedge mclk) begin
    if (mhsreset) begin
	wb_rd_rsp_q1[0]	<= '0;
	wb_rd_rsp_q2[0]	<= '0;
	wb_rd_rsp_q3[0]	<= '0;
	wb_rd_rsp_q1[1]	<= '0;
	wb_rd_rsp_q2[1]	<= '0;
	wb_rd_rsp_q3[1]	<= '0;
    end
    else begin
	wb_rd_rsp_q1[0]	<= wb_rd_rsp[0];
	wb_rd_rsp_q2[0]	<= wb_rd_rsp_q1[0];
	wb_rd_rsp_q3[0]	<= wb_rd_rsp_q2[0];
	wb_rd_rsp_q1[1]	<= wb_rd_rsp[1];
	wb_rd_rsp_q2[1]	<= wb_rd_rsp_q1[1];
	wb_rd_rsp_q3[1]	<= wb_rd_rsp_q2[1];
    end
end

assign o_wb_rd_rsp[0] = wb_rd_rsp_q1[0];
assign o_wb_rd_rsp[1] = wb_rd_rsp_q1[1];

assign o_wb_rd_data[0] = (wb_rd_rsp_q3[0].vld) ? eb_wr_data_save[0] : '0;
assign o_wb_rd_data[1] = (wb_rd_rsp_q3[1].vld) ? eb_wr_data_save[1] : '0;


// register wb_rd_req

msh_row_rd_req_t wb_rd_req_q1  [NUM_MSH_PLANES-1:0];
msh_row_rd_req_t wb_rd_req_q2  [NUM_MSH_PLANES-1:0];
msh_row_rd_req_t wb_rd_req_q3  [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
        wb_rd_req_q1[0] <= '0;
        wb_rd_req_q2[0] <= '0;
        wb_rd_req_q3[0] <= '0;
        wb_rd_req_q1[1] <= '0;
        wb_rd_req_q2[1] <= '0;
        wb_rd_req_q3[1] <= '0;
    end
    else begin
        wb_rd_req_q1[0] <= i_wb_rd_req[0];
        wb_rd_req_q2[0] <= wb_rd_req_q1[0];
        wb_rd_req_q3[0] <= wb_rd_req_q2[0];
        wb_rd_req_q1[1] <= i_wb_rd_req[1];
        wb_rd_req_q2[1] <= wb_rd_req_q1[1];
        wb_rd_req_q3[1] <= wb_rd_req_q2[1];
    end
end;

msh_row_rd_rsp_t eb_rd_rsp [NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t eb_rd_rsp_q1 [NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t eb_rd_rsp_q2 [NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t eb_rd_rsp_q3 [NUM_MSH_PLANES-1:0];

assign eb_rd_rsp[0].vld = wb_rd_req_q3[0].vld;
assign eb_rd_rsp[0].id = wb_rd_req_q3[0].id;

assign eb_rd_rsp[1].vld = wb_rd_req_q3[1].vld;
assign eb_rd_rsp[1].id = wb_rd_req_q3[1].id;

always_ff @(posedge mclk) begin
    if (mhsreset) begin
        eb_rd_rsp_q1[0] <= '0;
        eb_rd_rsp_q2[0] <= '0;
        eb_rd_rsp_q3[0] <= '0;
        eb_rd_rsp_q1[1] <= '0;
        eb_rd_rsp_q2[1] <= '0;
        eb_rd_rsp_q3[1] <= '0;
    end
    else begin
        eb_rd_rsp_q1[0] <= eb_rd_rsp[0];
        eb_rd_rsp_q2[0] <= eb_rd_rsp_q1[0];
        eb_rd_rsp_q3[0] <= eb_rd_rsp_q2[0];
        eb_rd_rsp_q1[1] <= eb_rd_rsp[1];
        eb_rd_rsp_q2[1] <= eb_rd_rsp_q1[1];
        eb_rd_rsp_q3[1] <= eb_rd_rsp_q2[1];
    end
end

assign o_eb_rd_rsp[0] = eb_rd_rsp_q1[0];
assign o_eb_rd_rsp[1] = eb_rd_rsp_q1[1];

assign o_eb_rd_data[0] = (eb_rd_rsp_q3[0].vld) ? wb_wr_data_save[0] : '0;
assign o_eb_rd_data[1] = (eb_rd_rsp_q3[1].vld) ? wb_wr_data_save[1] : '0;


// register nb_rd_req

msh_col_rd_req_t nb_rd_req_q1  [NUM_MSH_PLANES-1:0];
msh_col_rd_req_t nb_rd_req_q2  [NUM_MSH_PLANES-1:0];
msh_col_rd_req_t nb_rd_req_q3  [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
        nb_rd_req_q1[0] <= '0;
        nb_rd_req_q2[0] <= '0;
        nb_rd_req_q3[0] <= '0;
        nb_rd_req_q1[1] <= '0;
        nb_rd_req_q2[1] <= '0;
        nb_rd_req_q3[1] <= '0;
    end
    else begin
        nb_rd_req_q1[0] <= i_nb_rd_req[0];
        nb_rd_req_q2[0] <= nb_rd_req_q1[0];
        nb_rd_req_q3[0] <= nb_rd_req_q2[0];
        nb_rd_req_q1[1] <= i_nb_rd_req[1];
        nb_rd_req_q2[1] <= nb_rd_req_q1[1];
        nb_rd_req_q3[1] <= nb_rd_req_q2[1];
    end
end;


msh_col_rd_rsp_t sb_rd_rsp [NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t sb_rd_rsp_q1 [NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t sb_rd_rsp_q2 [NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t sb_rd_rsp_q3 [NUM_MSH_PLANES-1:0];

assign sb_rd_rsp[0].vld = nb_rd_req_q3[0].vld;
assign sb_rd_rsp[0].id = nb_rd_req_q3[0].id;
assign sb_rd_rsp[0].port_side = nb_rd_req_q3[0].port_side;
assign sb_rd_rsp[0].port_row = nb_rd_req_q3[0].port_row;

assign sb_rd_rsp[1].vld = nb_rd_req_q3[1].vld;
assign sb_rd_rsp[1].id = nb_rd_req_q3[1].id;
assign sb_rd_rsp[1].port_side = nb_rd_req_q3[1].port_side;
assign sb_rd_rsp[1].port_row = nb_rd_req_q3[1].port_row;


always_ff @(posedge mclk) begin
    if (mhsreset) begin
        sb_rd_rsp_q1[0] <= '0;
        sb_rd_rsp_q2[0] <= '0;
        sb_rd_rsp_q3[0] <= '0;
        sb_rd_rsp_q1[1] <= '0;
        sb_rd_rsp_q2[1] <= '0;
        sb_rd_rsp_q3[1] <= '0;
    end
    else begin
        sb_rd_rsp_q1[0] <= sb_rd_rsp[0];
        sb_rd_rsp_q2[0] <= sb_rd_rsp_q1[0];
        sb_rd_rsp_q3[0] <= sb_rd_rsp_q2[0];
        sb_rd_rsp_q1[1] <= sb_rd_rsp[1];
        sb_rd_rsp_q2[1] <= sb_rd_rsp_q1[1];
        sb_rd_rsp_q3[1] <= sb_rd_rsp_q2[1];
    end
end

assign o_sb_rd_rsp[0] = sb_rd_rsp_q1[0];
assign o_sb_rd_rsp[1] = sb_rd_rsp_q1[1];

assign o_sb_rd_data[0] = (sb_rd_rsp_q3[0].vld) ? nb_wr_data_save[0] : '0;
assign o_sb_rd_data[1] = (sb_rd_rsp_q3[1].vld) ? nb_wr_data_save[1] : '0;


// register sb_rd_req

msh_col_rd_req_t sb_rd_req_q1  [NUM_MSH_PLANES-1:0];
msh_col_rd_req_t sb_rd_req_q2  [NUM_MSH_PLANES-1:0];
msh_col_rd_req_t sb_rd_req_q3  [NUM_MSH_PLANES-1:0];

always_ff @(posedge mclk) begin
    if (mhsreset) begin
        sb_rd_req_q1[0] <= '0;
        sb_rd_req_q2[0] <= '0;
        sb_rd_req_q3[0] <= '0;
        sb_rd_req_q1[1] <= '0;
        sb_rd_req_q2[1] <= '0;
        sb_rd_req_q3[1] <= '0;
    end
    else begin
        sb_rd_req_q1[0] <= i_sb_rd_req[0];
        sb_rd_req_q2[0] <= sb_rd_req_q1[0];
        sb_rd_req_q3[0] <= sb_rd_req_q2[0];
        sb_rd_req_q1[1] <= i_sb_rd_req[1];
        sb_rd_req_q2[1] <= sb_rd_req_q1[1];
        sb_rd_req_q3[1] <= sb_rd_req_q2[1];
    end
end;


msh_col_rd_rsp_t nb_rd_rsp [NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t nb_rd_rsp_q1 [NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t nb_rd_rsp_q2 [NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t nb_rd_rsp_q3 [NUM_MSH_PLANES-1:0];

assign nb_rd_rsp[0].vld = sb_rd_req_q3[0].vld;
assign nb_rd_rsp[0].id = sb_rd_req_q3[0].id;
assign nb_rd_rsp[0].port_side = sb_rd_req_q3[0].port_side;
assign nb_rd_rsp[0].port_row = sb_rd_req_q3[0].port_row;

assign nb_rd_rsp[1].vld = sb_rd_req_q3[1].vld;
assign nb_rd_rsp[1].id = sb_rd_req_q3[1].id;
assign nb_rd_rsp[1].port_side = sb_rd_req_q3[1].port_side;
assign nb_rd_rsp[1].port_row = sb_rd_req_q3[1].port_row;


always_ff @(posedge mclk) begin
    if (mhsreset) begin
        nb_rd_rsp_q1[0] <= '0;
        nb_rd_rsp_q2[0] <= '0;
        nb_rd_rsp_q3[0] <= '0;
        nb_rd_rsp_q1[1] <= '0;
        nb_rd_rsp_q2[1] <= '0;
        nb_rd_rsp_q3[1] <= '0;
    end
    else begin
        nb_rd_rsp_q1[0] <= nb_rd_rsp[0];
        nb_rd_rsp_q2[0] <= nb_rd_rsp_q1[0];
        nb_rd_rsp_q3[0] <= nb_rd_rsp_q2[0];
        nb_rd_rsp_q1[1] <= nb_rd_rsp[1];
        nb_rd_rsp_q2[1] <= nb_rd_rsp_q1[1];
        nb_rd_rsp_q3[1] <= nb_rd_rsp_q2[1];
    end
end

assign o_nb_rd_rsp[0] = nb_rd_rsp_q1[0];
assign o_nb_rd_rsp[1] = nb_rd_rsp_q1[1];

assign o_nb_rd_data[0] = (nb_rd_rsp_q3[0].vld) ? sb_wr_data_save[0] : '0;
assign o_nb_rd_data[1] = (nb_rd_rsp_q3[1].vld) ? sb_wr_data_save[1] : '0;



endmodule // mby_msh_node
