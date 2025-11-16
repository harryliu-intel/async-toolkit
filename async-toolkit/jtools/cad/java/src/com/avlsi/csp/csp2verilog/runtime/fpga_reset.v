// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef FPGA_HIER_PATH
`define FPGA_HIER_PATH .
`endif

module fpga_reset #(parameter RESETS = 1, STARTS = 0, DELAYS = 0)
                  (output logic [RESETS+STARTS+DELAYS-1:0] reset_n);
logic clk;

initial begin
  reset_n = '0;
  clk     = 0;
  #10
  reset_n[RESETS-1:0] = '1;
  if (STARTS > 0) begin
    #10;
    for (int i = RESETS; i < RESETS+STARTS; i++) begin
      reset_n[i] = '1;
    end
  end
end

initial forever #1 clk = ~clk;

buf (TESTBENCH.x `FPGA_HIER_PATH _$0.CLK, clk);

endmodule
