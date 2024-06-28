
// W-bit saturating counter, configurable count range
// range is [ 0 .. MAXVAL ]
// saturates at ends of range
module incdec
  #(parameter W=8,
    parameter MAXVAL=160)
   (
    input  logic [W-1:0]  i_cur,
    input  logic          i_decrement,
    output logic [W-1:0]  o_nxt
   );

   always_comb begin
     
      o_nxt = i_cur;

      if (i_decrement)
        o_nxt = (i_cur == 0)      ? 0      : (i_cur - 1);
      else
        o_nxt = (i_cur == MAXVAL) ? MAXVAL : (i_cur + 1);
   end
endmodule
