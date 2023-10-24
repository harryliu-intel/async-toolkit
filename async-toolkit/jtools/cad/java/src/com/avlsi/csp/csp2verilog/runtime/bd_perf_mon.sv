module bd_perf_mon (
  input CLK,
  input DLY,
  input rb, 
  input \L.a ,
  input \L.q ,
  input \R.a ,
  input \R.q );

//Check if L,R channels are active or idle
logic Lidle, Ridle;
always_comb begin
  Lidle = (\L.q === \L.a );
  Ridle = (\R.q === \R.a );
end

final begin
  if (Lidle == 1'b0) $display("%m L channel not idle\n");
  if (Ridle == 1'b0) $display("%m R channel not idle\n");
end

endmodule


