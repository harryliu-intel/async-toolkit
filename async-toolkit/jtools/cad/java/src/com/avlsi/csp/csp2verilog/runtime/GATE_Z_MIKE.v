module GATE_Z_MIKE (out, up, down);
  `PRS2VERILOG_TIMESCALE
  parameter updelay = 0;
  parameter dndelay = 0;
  output out;
  input up, down;
  wire out, up, down;

  bufif1 #(updelay,dndelay,0) bufifup(out, 1'b1, up);
  bufif1 #(updelay,dndelay,0) bufifdown(out, 1'b0, down);
endmodule

module GATE_Z_MIKE_TRIREG (out, up, down);
  `PRS2VERILOG_TIMESCALE
  parameter updelay = 0;
  parameter dndelay = 0;
  output out;
  input up, down;
  trireg out;
  wire up, down;

  bufif1 #(updelay,dndelay,0) bufifup(out, 1'b1, up);
  bufif1 #(updelay,dndelay,0) bufifdown(out, 1'b0, down);
endmodule

module GATE_Z_MIKE_CLK (out, up, down, clk);
  `PRS2VERILOG_TIMESCALE
  parameter updelay = 0;
  parameter dndelay = 0;
  output out;
  input up, down, clk;
  wire out, up, down, clk;

  bufif1 #(updelay,dndelay,0) bufifup(out, 1'b1, up);
  bufif1 #(updelay,dndelay,0) bufifdown(out, 1'b0, down);
endmodule

module GATE_Z_MIKE_CLK_TRIREG (out, up, down, clk);
  `PRS2VERILOG_TIMESCALE
  parameter updelay = 0;
  parameter dndelay = 0;
  output out;
  input up, down, clk;
  trireg out;
  wire up, down, clk;

  bufif1 #(updelay,dndelay,0) bufifup(out, 1'b1, up);
  bufif1 #(updelay,dndelay,0) bufifdown(out, 1'b0, down);
endmodule
