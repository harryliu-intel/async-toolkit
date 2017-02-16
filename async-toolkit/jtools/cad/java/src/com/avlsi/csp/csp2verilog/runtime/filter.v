/************************************************************
 * This verilog model for FILTER prevents both nodes of the
 * metastable pair _D0,_D1 from falling simultaneously,
 * by forcing _D1 high in that case.  This problem does not
 * occur in dsim, but in verilog it can happen when
 * both transitions are scheduled at the same time.
 *
 * Added functionality for Demonic behavior as per Bug3149
 * Filter will only allow one port to be serviced until there are
 * no more requests on that port and a timer expires.
 * See bug for more details.
 *
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id: //hw/tsmc28-dev/verilog/lib/metastable/lib_metastable_filter_FILTER.v#1 $
 * 
 ************************************************************/

module  \lib.metastable.primitive.filter.FILTER  (D0,D1, GND,Vdd, _D0,_D1);
  inout  _D0,_D1;
  output D0,D1;
  input  GND,Vdd;

  wire  D0,D1,_D0,_D1;
  wire  GND,Vdd;

  reg [3:0] arb_mode;
  reg [31:0] arb_delay;
  integer timer;
  wire expired;
  reg [1:0] own;

  initial begin
	own = 2'b11;
	arb_mode = 4'b1;
	arb_delay = 32'b0;
	#1
	timer = 1;
   end

  // resolve metastability
  always @(_D0 or _D1)
  if (arb_mode[0] || (arb_mode==4'b0)) begin	// default behavior
    if (~_D0 & ~_D1)
       force _D1=1;
    else if (_D0)
       release _D1;
  end
  else if (arb_mode[1]) begin			// both ports stuck
    if (!_D0 && own[0])
       force _D1=1;
    else if (!_D1 && own[1])
       force _D0=1;
  end
  else if (arb_mode[2]) begin			// port 0 stuck
    if (!_D0)
       force _D1=1;
  end
  else if (arb_mode[3]) begin			// port 1 stuck
    if (!_D1)
       force _D0=1;
  end
   
  

  not #((`PRS2VERILOG_TAU) * 1.25, (`PRS2VERILOG_TAU) * 0.75) gate_D0 (D0,_D0);
  not #((`PRS2VERILOG_TAU) * 1.25, (`PRS2VERILOG_TAU) * 0.75) gate_D1 (D1,_D1);


task reset_timer;
   begin
	timer = arb_delay;
   end
endtask

   // count down to 0
initial begin
   #1 ;
   if (!arb_mode[0])
	forever #1 timer = timer - 1;
end

assign expired = (timer==0) & ~arb_mode[0];

   // arbitration
always @(posedge expired) begin
   release _D0;
   release _D1;
   own = 2'b11;
   wait (!_D0 || !_D1) ;
   if (!_D0) own = 2'b01;
   else if (!_D1) own = 2'b10;
end

   // reset timer on every request
always @(_D0 or _D1) if (!_D0 || !_D1) reset_timer;

endmodule
