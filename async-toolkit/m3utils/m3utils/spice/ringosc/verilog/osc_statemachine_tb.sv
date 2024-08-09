
`timescale 1ps/1fs

`include "osc_statemachine.sv"

module osc_statemachine_tb #() ();
   localparam NSETTINGS=4;
   
   localparam NINTERP  =8;
   
   localparam NSTAGES  =6;
   
   localparam NTAPS    = NSTAGES - 1;
   
   localparam NSETTINGS_PER_TAP = NINTERP * NSETTINGS;
   
   localparam NTOTAL_SPEEDS = NTAPS * NSETTINGS_PER_TAP + 1;
   
   localparam W        = $clog2(NTOTAL_SPEEDS);
   

   logic                                          clk;
   logic                                          rst_n;
   logic                                          i_up_down;
   logic [ W - 1 : 0 ]                            i_rstval;
   logic [ NSTAGES - 1 : 0]                       o_stage_en;
   logic [ NINTERP - 1 : 0][ NSETTINGS - 1 : 0 ]  o_interp_ctrl;
   
   
   osc_statemachine #() u_dut (.*);

   initial begin
      clk = '0;

      while(1)
        begin
           #500;
           clk = ~clk;

        end
   end
   
   initial begin
      rst_n = '0;
      @(posedge clk);
      @(posedge clk);
      @(posedge clk);
      @(posedge clk);
      @(posedge clk);
      rst_n = '1;
   end

   initial begin
      i_rstval = 127;
   end
   
   initial begin
      for (int ud=0; ud < 1 + 1; ++ud)
        for (int i=0; i <NTOTAL_SPEEDS + 2; ++i) begin

           @(negedge clk);
           
           i_up_down = ud;

           @(posedge clk);
           
           $display("i_up_down = %b, o_stage_en=%b, o_interp_ctrl=%b",
                    i_up_down, o_stage_en, o_interp_ctrl);
           
        
        end
   end
endmodule
     

   
