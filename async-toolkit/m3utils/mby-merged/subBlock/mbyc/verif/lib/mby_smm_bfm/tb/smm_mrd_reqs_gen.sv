
module smm_row_mrd_reqs #(parameter TIME_DELAY = 625,parameter ADDRESS_BITS=20, parameter WORD_BYTES=64)(
    input mesh_clk,
    input mesh_rst,
    mby_smm_bfm_row_rd_req_if mrd_req,
    input [ADDRESS_BITS-1:0] i_rd_seg_ptr,
    input [1:0] i_rd_wd_sel
    
    );

   logic [7:0] data_ctr;
   logic [12:0] req_id;

initial begin
   $monitor ("%g MemRd address=%h",$time, mrd_req.intf_data_pkt.mim_seg_ptr);
   mrd_req.intf_data_pkt.mim_rreq_valid = 0;
   //data_ctr = 0;
   //mrd_req.intf_data_pkt.wr_dbus.data = {WORD_BYTES*8{1'b0}};
   mrd_req.intf_data_pkt.mim_seg_ptr  = {ADDRESS_BITS{1'b0}};
   req_id = 0;
   
    @(negedge mesh_rst);
    #100;
    repeat(10) begin
       @(posedge mesh_clk);
	    data_ctr++; 
       mrd_req.intf_data_pkt.mim_rreq_valid = 1;
       mrd_req.intf_data_pkt.mim_seg_ptr = i_rd_seg_ptr;
       mrd_req.intf_data_pkt.mim_wd_sel  = i_rd_wd_sel;
       mrd_req.intf_data_pkt.mim_req_id = req_id;
       req_id++;
       @(posedge mesh_clk);
       mrd_req.intf_data_pkt.mim_rreq_valid = 0;
    end //repeat for write
end //initial

    
endmodule // smm_row_mrd_reqs
