// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
	Created by Mike Miller 5/15/03
	This is to model correct reset behavior for cross coupled init
	and x outputs
	-- 5/21/03 Updated to match delay model of READ BIT CELL
	-- 5/23/03 Updated for iw late deassertion (see bug 2541)
*/

module
\lib.sram.10T.BIT  (
GND , Reset , Vdd ,
init0 , init1 ,
ir , iw , jre ,
x0 , x1 ,
_r0 , _r1 ,
_w0 , _w1 );
input GND ;
input Reset ;
input Vdd ;
output init0 ;
output init1 ;
input ir ;
input iw ;
input jre ;
output x0 ;
output x1 ;
output _r0 ;
output _r1 ;
input _w0 ;
input _w1 ;

trireg _r0, _r1 ;
trireg x0, x1 ;
reg rx0, rx1 ;
wire dr0, dr1;

assign dr0 = (jre && ir && x0) ? 1'b1 : 1'b0;
assign dr1 = (jre && ir && x1) ? 1'b1 : 1'b0;

bufif1 #(((`PRS2VERILOG_TAU) * 1.25), ((`PRS2VERILOG_TAU) * 0.75)) (_r0, 1'b0, dr0);
bufif1 #(((`PRS2VERILOG_TAU) * 1.25), ((`PRS2VERILOG_TAU) * 0.75)) (_r1, 1'b0, dr1);

assign x0 = (Reset) ? 1'bz : rx0;
assign x1 = (Reset) ? 1'bz : rx1;

always @(iw or _w0 or _w1 or Reset or x0 or x1)
  if (Reset) begin
	rx0 <= x0;
	rx1 <= x1;
  end
  else if (iw && !_w1 && _w0) begin
	rx0 <= 1'b0;
	rx1 <= 1'b1;
  end
  else if (iw && _w1 && !_w0) begin
	rx0 <= 1'b1;
	rx1 <= 1'b0;
  end
  else if (iw && !_w1 && !_w0) begin
	rx0 <= 1'bx;
	rx1 <= 1'bx;
  end

assign init0 = (Reset) ? 1'b1 : 1'bz;
assign init1 = (Reset) ? 1'b0 : 1'bz;

endmodule
