// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0


module smm_row_mwr_reqs #(parameter TIME_DELAY = 625,parameter ADDRESS_BITS=20, parameter WORD_BYTES=64)(
    input valid,
    input [ADDRESS_BITS-1:0] seg_ptr,
    input mesh_clk,
    input mesh_rst,
    mby_smm_bfm_mwr_req_if mwr_req
    );

    logic [7:0] data_ctr;
    logic [1:0] wd_sel_ctr;

initial begin
    $monitor ("%g MemWr seg_ptr=%h data=%h",$time, mwr_req.intf_data_pkt.mim_wr_seg_ptr, mwr_req.intf_data_pkt.mim_wr_data);
    mwr_req.intf_data_pkt.mim_wreq_valid = 0;
    data_ctr = 0;
    wd_sel_ctr = 0;
   
    mwr_req.intf_data_pkt.mim_wr_data = {WORD_BYTES*8{1'b0}};
    @(negedge mesh_rst);
    #100;
    forever begin
        @(posedge mesh_clk);
        mwr_req.intf_data_pkt.mim_wreq_valid = 0;
        if(valid) begin
           data_ctr++; 
           mwr_req.intf_data_pkt.mim_wreq_valid = 1;
           mwr_req.intf_data_pkt.mim_wr_data = {64{data_ctr}};
           mwr_req.intf_data_pkt.mim_wr_seg_ptr = seg_ptr;
           mwr_req.intf_data_pkt.mim_wr_wd_sel = wd_sel_ctr%2;
           wd_sel_ctr++;
           @(posedge mesh_clk);
           mwr_req.intf_data_pkt.mim_wr_data = {8{{16'h0000},{16'hbebe},{2{data_ctr}},{16'hcafe}}}; //metadata=0000-bebe-datactnr-cafe :concat with data cntr to know what md is associated to data
           mwr_req.intf_data_pkt.mim_wr_wd_sel = wd_sel_ctr%2;
           wd_sel_ctr++;
        end
    end //repeat for write
end //initial

    
endmodule // smm_row_mwr_reqs
