module fpga_sclk;
reg S_CLK;
assign TESTBENCH.x._$0.S_CLK = S_CLK;
initial begin
  S_CLK = 1'b0;
  forever #5ps S_CLK = ~S_CLK;
end
endmodule
