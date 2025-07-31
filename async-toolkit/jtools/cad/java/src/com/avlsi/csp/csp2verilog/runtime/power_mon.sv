`timescale 1ps/1fs

module power_mon #(string name) (input logic signal);

realtime t_sig;
logic prev_sig;
int file;

initial begin
  file = $fopen($sformatf("power_%s.time", name), "w");
  $timeformat(-12,0,"ps");
  prev_sig <= signal;
end

always @(signal) begin
  if (prev_sig === 1'b0 || prev_sig === 1'b1) begin
    $fdisplay(file, "%0t", $realtime);
  end
  prev_sig = signal;
end

final begin
  $fclose(file);
end

endmodule
