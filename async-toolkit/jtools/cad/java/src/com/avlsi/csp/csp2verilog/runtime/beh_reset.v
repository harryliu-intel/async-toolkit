`ifndef CAST2VERILOG_RESET_DURATION
`define CAST2VERILOG_RESET_DURATION 10
`endif
`ifndef CAST2VERILOG_START_DURATION
`define CAST2VERILOG_START_DURATION 10
`endif
module beh_reset #(parameter RESETS = 1,
                             STARTS = 0,
                             STEPS = 0,
                             DELAYS = 0,
                             CAPTURES = 0,
                             CUTSCANS = 0)
                  (output logic [RESETS+STARTS+STEPS+DELAYS+CAPTURES+CUTSCANS-1:0] reset_n);
initial begin
  reset_n = '0;
  if (DELAYS > 0) begin
    int dly = 0;
    if ($test$plusargs("DLY")) begin
      $value$plusargs("DLY=%d", dly);
      $display("%m: DLY set to %d", dly);
    end
    for (int i = RESETS+STARTS+STEPS; i < RESETS+STARTS+STEPS+DELAYS; i++) begin
      reset_n[i] = dly[0];
    end
  end
  #`CAST2VERILOG_RESET_DURATION;
  reset_n[RESETS-1:0] = '1;
  if (STARTS+STEPS+CAPTURES > 0) begin
    #`CAST2VERILOG_START_DURATION;
    for (int i = RESETS; i < RESETS+STARTS+STEPS; i++) begin
      reset_n[i] = '1;
    end
    for (int i = RESETS+STARTS+STEPS+DELAYS; i < RESETS+STARTS+STEPS+DELAYS+CAPTURES; i++) begin
      reset_n[i] = '1;
    end
  end
end

endmodule
