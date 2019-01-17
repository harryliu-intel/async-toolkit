
module smm_row_mwr_reqs #(parameter TIME_DELAY = 625,parameter ADDRESS_BITS=20, parameter WORD_BYTES=64)(
    input valid,
    input [ADDRESS_BITS-1:0] seg_ptr,
    input mesh_clk,
    input mesh_rst,
    mby_smm_bfm_row_wr_req_if mwr_req
    //output reg [2:0] o_node_col,
    //output reg [3:0] o_node_row,
    //output reg [ADDRESS_BITS-1:0] o_wr_seg_ptr,
    //output reg [1:0] o_wr_wd_sel
    );

    logic [7:0] data_ctr;
    logic [1:0] wd_sel_ctr;

initial begin
    $monitor ("%g MemWr seg_ptr=%h data=%h",$time, mwr_req.intf_data_pkt.mim_wr_seg_ptr, mwr_req.intf_data_pkt.mim_wr_data);
    mwr_req.intf_data_pkt.mim_wreq_valid = 0;
    //mwr_req.node_col=0;
    //mwr_req.node_row=0;
    data_ctr = 0;
    wd_sel_ctr = 0;
   
    mwr_req.intf_data_pkt.mim_wr_data = {WORD_BYTES*8{1'b0}};
    //mwr_req.intf_data_pkt.mim_wr_seg_ptr  = {ADDRESS_BITS{1'b0}};
    @(negedge mesh_rst);
    #100;
    //repeat(10) begin
    forever begin
        @(posedge mesh_clk);
        if(valid) begin
	        data_ctr++; 
           mwr_req.intf_data_pkt.mim_wreq_valid = 1;
           //mwr_req.intf_data_pkt.wr_req.node_col = ($random%8);
           //o_node_col = mwr_req.intf_data_pkt.wr_req.node_col;
           //mwr_req.intf_data_pkt.wr_req.node_row = ($random%16);
           //o_node_row = mwr_req.intf_data_pkt.wr_req.node_row;
           mwr_req.intf_data_pkt.mim_wr_data = {64{data_ctr}};
           //if(wd_sel_ctr == 0)
           //   begin
           //mwr_req.intf_data_pkt.mim_wr_seg_ptr = ($random%(1<<ADDRESS_BITS));
           //      o_wr_seg_ptr = mwr_req.intf_data_pkt.mim_wr_seg_ptr;
           //   end
           //else
           //   begin
           mwr_req.intf_data_pkt.mim_wr_seg_ptr = seg_ptr;
           //   end   
           mwr_req.intf_data_pkt.mim_wr_wd_sel = wd_sel_ctr;
           //o_wr_wd_sel  = mwr_req.intf_data_pkt.mim_wr_wd_sel;
           @(posedge mesh_clk);
           wd_sel_ctr++;
           mwr_req.intf_data_pkt.mim_wreq_valid = 0;
           @(posedge mesh_clk);
           data_ctr++;
           mwr_req.intf_data_pkt.mim_wreq_valid = 1;
           mwr_req.intf_data_pkt.mim_wr_data = {64{data_ctr}};
           mwr_req.intf_data_pkt.mim_wr_wd_sel = wd_sel_ctr;
           @(posedge mesh_clk);
           wd_sel_ctr++;
           mwr_req.intf_data_pkt.mim_wreq_valid = 0;
        end
	//#($random%10);
    end //repeat for write
end //initial

    
endmodule // smm_row_mwr_reqs
