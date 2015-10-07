module csp_e1of_widenarrow #(parameter integer BASE=4, LEN=2, NBITS=3, WBITS=5)
                            (output reg signed [0:LEN-1][NBITS-1:0] NARROW$data,
                             input [0:LEN-1] NARROW$enable,
                             input signed [WBITS-1:0] WIDE$data,
                             output reg WIDE$enable);
always @* begin
  unique case (1)
     &(NARROW$enable): WIDE$enable = 1;
    ~|(NARROW$enable): WIDE$enable = 0;
  endcase
end

always @* begin
  if (WIDE$data >= 0) begin
    reg signed [WBITS-1:0] x;
    x = WIDE$data;
    for (int i = 0; i < LEN; i++) begin
      NARROW$data[i] = x % BASE; x = x / BASE;
    end
  end
  else begin
    NARROW$data = '1;
  end
end
endmodule

module csp_e1of_narrowwide #(parameter integer BASE=4, LEN=2, NBITS=3, WBITS=5)
                            (input signed [0:LEN-1][NBITS-1:0] NARROW$data,
                             output [0:LEN-1] NARROW$enable,
                             output reg signed [WBITS-1:0] WIDE$data,
                             input WIDE$enable);
wire [LEN-1:0] signbits;
assign NARROW$enable = {LEN{WIDE$enable}};
generate for (genvar i = 0; i < LEN; i++)
  assign signbits[i] = NARROW$data[i][NBITS-1];
endgenerate
always @* begin
  unique case (1)
    &(signbits): WIDE$data = -1;
   ~|(signbits): begin
                   reg signed [WBITS-1:0] x;
                   x = 0;
                   for (int i = LEN - 1; i >= 0; i--) begin
                     x = BASE * x + NARROW$data[i];
                   end
                   WIDE$data = x;
                 end
  endcase
end
endmodule
