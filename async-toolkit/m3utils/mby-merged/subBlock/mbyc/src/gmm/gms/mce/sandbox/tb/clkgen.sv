module clkgen(output logic clk);

parameter FREQ_MHZ = 100.0;

initial begin
    clk = 0;
    forever #(1.0us/FREQ_MHZ/2.0) clk = !clk;
end

endmodule
