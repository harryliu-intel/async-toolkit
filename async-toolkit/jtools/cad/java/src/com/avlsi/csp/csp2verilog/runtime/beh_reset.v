module beh_reset #(parameter RESETS = 1, STARTS = 0, DELAYS = 0)
                  (output logic [RESETS+STARTS+DELAYS-1:0] reset_n);
initial begin
  reset_n = '0;
  #10;
  reset_n[RESETS-1:0] = '1;
  if (STARTS > 0) begin
    #10;
    for (int i = RESETS; i < RESETS+STARTS; i++) begin
      reset_n[i] = '1;
    end
  end
end

endmodule
