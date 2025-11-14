// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef CAST2VERILOG_RESET_DURATION
`define CAST2VERILOG_RESET_DURATION 20000ns
`endif
`ifndef CAST2VERILOG_START_DURATION
`define CAST2VERILOG_START_DURATION 100ns
`endif
`ifndef CAST2VERILOG_CAPTURE_DURATION
`define CAST2VERILOG_CAPTURE_DURATION 1000ns
`endif
module beh_reset #(parameter RESETS = 1,
                             STARTS = 1,
                             STEPS = 0,
                             DELAYS = 0,
                             CAPTURES = 0,
                             CUTSCANS = 0,
                             PASSTHRUS = 0,
                             INJECTS = 0)
                  (output logic [RESETS+STARTS+STEPS+DELAYS+CAPTURES+CUTSCANS+PASSTHRUS+INJECTS-1:0] reset_n);
parameter RESET_begin    = 0;
parameter START_begin    = RESET_begin+RESETS;
parameter STEP_begin     = START_begin+STARTS;
parameter DELAY_begin    = STEP_begin+STEPS;
parameter CAPTURE_begin  = DELAY_begin+DELAYS;
parameter CUTSCAN_begin  = CAPTURE_begin+CAPTURES;
parameter PASSTHRU_begin = CUTSCAN_begin+CUTSCANS;
parameter INJECT_begin   = PASSTHRU_begin+PASSTHRUS;
initial begin
`define SET_NODES(n, v) if (n``S > 0) for (int i=0; i<n``S; ++i) reset_n[n``_begin+i] = v;
  reset_n = '0;
  if (DELAYS > 0) begin
    int dly = 0;
    if ($test$plusargs("DLY")) begin
      $value$plusargs("DLY=%d", dly);
      $display("%m: DLY set to %d", dly);
    end
    `SET_NODES(DELAY, dly[0]);
  end
  `SET_NODES(PASSTHRU, 1'b1);
  #`CAST2VERILOG_RESET_DURATION;
  `SET_NODES(RESET, 1'b1);
  if (STARTS+STEPS+CAPTURES > 0) begin
    #`CAST2VERILOG_CAPTURE_DURATION;
  end
  if (CAPTURES > 0) begin
    `SET_NODES(CAPTURE, 1'b1);
    if (STARTS+STEPS > 0) begin
      #`CAST2VERILOG_START_DURATION;
    end
  end
  `SET_NODES(START, 1'b1);
  `SET_NODES(STEP, 1'b1);
end

endmodule
