/* Created by Mike Miller */
module
\lib.sram.6T.BIT (GND, Vdd, _Reset, a, b0, b1, en);
input GND;
input Vdd;
input _Reset;
input a;
output b0 ;
output b1 ;
input en;

wire b0, b1;
trireg x0;

assign #(((`PRS2VERILOG_TAU) * 1.25), ((`PRS2VERILOG_TAU) * 0.75)) 
       x0 = (!_Reset)  ? $random & 1'b1 :
            (a && !b0) ? 1'b0 :
            (a && !b1) ? 1'b1 :
                         1'bz;

assign #(((`PRS2VERILOG_TAU) * 1), ((`PRS2VERILOG_TAU) * 6.75))
       b0 = (a && en && b1 && !x0) ? 1'b0 :
                                     1'bz;

assign #(((`PRS2VERILOG_TAU) * 1), ((`PRS2VERILOG_TAU) * 6.75))
       b1 = (a && en && b0 && x0) ? 1'b0 :
                                    1'bz;
 
endmodule
