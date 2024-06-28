
`timescale 1ps/1fs

`include "oscdecode.sv"

module oscdecode_tb #() ();
   localparam NSETTINGS=4;
   
   localparam NINTERP  =8;
   
   localparam NSTAGES  =6;
   
   localparam NTAPS    = NSTAGES - 1;
   
   localparam NSETTINGS_PER_TAP = NINTERP * NSETTINGS;
   
   localparam NTOTAL_SPEEDS = NTAPS * NSETTINGS_PER_TAP + 1;
   
   localparam W        = $clog2(NTOTAL_SPEEDS);
   

   logic [ W - 1 : 0 ]      i_speed;
   logic [ NSTAGES - 1 : 0] o_stage_en;
   logic [ NINTERP - 1 : 0][ NSETTINGS - 1 : 0 ] o_interp_ctrl;
   
   oscdecode #() u_dut (.*);

   initial begin
      for (int i=0; i <NTOTAL_SPEEDS; ++i) begin
         i_speed = i;
         
         #1;
         
         $display("i_speed=%d, o_stage_en=%b, o_interp_ctrl=%b",
                  i_speed, o_stage_en, o_interp_ctrl);
         
      end
        
   end
endmodule
     

   
