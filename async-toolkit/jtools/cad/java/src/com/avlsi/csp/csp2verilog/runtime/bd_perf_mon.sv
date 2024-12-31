
`timescale 1ps/1fs

`define MAX2(t1, t2) ((t1) > (t2) ? (t1) : (t2))
`define MAX3(t1, t2, t3) `MAX2((t1), `MAX2((t2), (t3)))

`define MIN2(t1, t2) ((t1) < (t2) ? (t1) : (t2))
`define MIN3(t1, t2, t3) `MIN2((t1), `MIN2((t2), (t3)))


module bd_ctrl_perf_mon (
  input logic CLK,
  input logic DLY,
  input logic rb,
  input logic \L.a ,
  input logic \L.q ,
  input logic \R.a ,
  input logic \R.q );


//Check if L,R channels are active or idle
logic Lidle, Ridle;
//Check if L,R channels are inverted (different idle condition)
logic Linv, Rinv;


always @ ( rb ) begin
  if (rb == 1'b0) begin
    Linv <= 1'b0;
    Rinv <= 1'b0;
  end
  else if (rb == 1'b1) begin
    //Just coming out of reset. Do Error checking
    if ( \L.q  === 1'bx) $display("%m: WARNING - L.q is X coming out of reset at %0t", $time);
    if ( \R.a  === 1'bx) $display("%m: WARNING - R.a is X coming out of reset at %0t", $time);
    //If there is an initial token, the channel will be inverted after reset is de-asserted
    if ( \L.q  == 1'b1) Linv <= 1'b1;
    if ( \R.a  == 1'b1) Rinv <= 1'b1;
  end
end

//Compute if L,R channels are idle, taking into account if the channel is inverted
always_comb begin
  Lidle = (((Linv==1'b1) ? !\L.q : \L.q ) == \L.a );
  Ridle = ( \R.q == ((Rinv==1'b1) ? !\R.a : \R.a ));
end

//Save the times of the last time signals transitioned
time t_Lq, t_La, t_Rq, t_Ra;
int n_Lq, n_La, n_Rq, n_Ra;
time max_Rq_Ra, max_Lq_Clk;
time t_Rq_Ra, t_Lq_Clk;
time tot_Rq_Ra, tot_Lq_Clk;
always @( \L.q ) begin
  t_Lq = (rb==1'b0) ? '0 : $time;
  n_Lq = (rb==1'b0) ? '0 : n_Lq+1;
end
always @( \L.a ) begin
  t_La = (rb==1'b0) ? '0 : $time;
  n_La = (rb==1'b0) ? '0 : n_La+1;
end
always @( \R.q ) begin
  t_Rq = (rb==1'b0) ? '0 : $time;
  n_Rq = (rb==1'b0) ? '0 : n_Rq+1;
end
always @( \R.a ) begin
  t_Ra = (rb==1'b0) ? 1'b0 : $time;
  n_Ra = (rb==1'b0) ? '0 : n_Ra+1;
end


//Measure R.q -> R.a
always @( \R.a  or rb ) begin
  if (rb == 1'b0) begin
    max_Rq_Ra = '0;
    tot_Rq_Ra = '0;
    t_Rq_Ra = '0;
  end
  else if (t_Rq > 0) begin
    tot_Rq_Ra += $time - t_Rq;
    if (max_Rq_Ra < $time - t_Rq) begin
      max_Rq_Ra = $time - t_Rq;
      t_Rq_Ra = $time;
    end
  end
end

//Measure L.q, R.a to Clk+
always @(posedge CLK or rb ) begin
  if (rb == 1'b0) begin
    max_Lq_Clk = '0;
    tot_Lq_Clk = '0;
    t_Lq_Clk = '0;
  end
  else begin
    if (t_Lq > 0) begin
      tot_Lq_Clk += $time - t_Lq;
      if (max_Lq_Clk < $time - t_Lq) begin
        max_Lq_Clk = $time - t_Lq;
        t_Lq_Clk = $time;
      end
    end
  end
end

integer file;
//Display results at the end of the simulation
final begin
  $timeformat(-12,0,"ps");
  if (Lidle == 1'b0 && Ridle == 1'b0) $display("%m: Both L&R channels are not idle, last activity at %0t", (t_Lq > t_Ra) ? t_Lq : t_Ra);
  else begin
    if (Lidle == 1'b0) $display("%m: L channel not idle, last activity at %0t", (t_Lq > t_La) ? t_Lq : t_La);
    if (Ridle == 1'b0) $display("%m: R channel not idle, last activity at %0t", (t_Rq > t_Ra) ? t_Rq : t_Ra);
  end
  //open a new file
  file = $fopen("bd_perf.rpt", "a");
  if (t_Rq_Ra > 0) begin
    $fdisplay(file, "%m: Avg R.q->R.a=%0t (%0d), Max R.q->R.a: %0t (%0.2f%%) @ %0t", tot_Rq_Ra/n_Rq, n_Rq, max_Rq_Ra, 100.0*(max_Rq_Ra-(tot_Rq_Ra/n_Rq))/(tot_Rq_Ra/n_Rq), t_Rq_Ra);
    $fdisplay(file, "%m: Avg L.q->Clk=%0t (%0d), Max L.q->Clk: %0t (%0.2f%%) @ %0t", tot_Lq_Clk/n_Lq, n_Lq, max_Lq_Clk, 100.0*(max_Lq_Clk-(tot_Lq_Clk/n_Lq))/(tot_Lq_Clk/n_Lq), t_Lq_Clk);
  end
  $fclose(file);
end

endmodule


module bd_div_perf_mon (
  input logic L,
  input logic S,
  input logic rb,
  input logic \R[0] ,
  input logic \R[1] );


always @ ( rb ) begin
  if (rb == 1'b1) begin
    //Just coming out of reset. Do Error checking
    //if ( L === 1'bx) $display("%m: WARNING - L is X coming out of reset at %0t", $time);
    //if ( S === 1'bx) $display("%m: WARNING - S is X coming out of reset at %0t", $time);
  end
end

always @ ( L ) begin
  if (rb == 1'b1 && L !== 1'bx && S === 1'bx) begin
    //Bad!! This will cause an xprop into the control path
    $display("%m: WARNING - S is X when L toggles at %0t", $time);
  end
end

//Instrumentation that is probably not needed
int n_L, n_R0, n_R1;
always @( L ) begin
  n_L = (rb==1'b0) ? '0 : n_L+1;
end
always @( \R[0] ) begin
  n_R0 = (rb==1'b0) ? '0 : n_R0+1;
end
always @( \R[1] ) begin
  n_R1 = (rb==1'b0) ? '0 : n_R1+1;
end

endmodule


module bd_ct2_perf_mon (
  input logic \A[0] ,
  input logic \A[1] ,
  input logic X
);

logic idle;
always_comb idle = ( \A[0]  == \A[1] );

//Save the times of the last time signals transitioned
time t_A0, t_A1;
int n_A0, n_A1;
time max_diff;
time t_diff;
time tot_diff;

always @( \A[0] ) begin
  t_A0 = (X === 1'bx) ? '0 : $time;
  n_A0 = (X === 1'bx) ? '0 : n_A0+1;
end
always @( \A[1] ) begin
  t_A1 = (X === 1'bx) ? '0 : $time;
  n_A1 = (X === 1'bx) ? '0 : n_A1+1;
end

time diff;
always @( X ) begin
  //diff = (t_A0 > t_A1) ? t_A0 - t_A1: t_A1 - t_A0;
  diff = `MAX2(t_A0, t_A1) - `MIN2(t_A0, t_A1);
  tot_diff += diff;
  if (max_diff < diff) begin
    max_diff = diff;
    t_diff = $time;
  end
end

integer file;
//Display results at the end of the simulation
final begin
  $timeformat(-12,0,"ps");
  if (idle == 1'b0) $display("%m: C-element inputs are not idenitical, last activity at %0t", `MAX2(t_A0, t_A1));
  //open a new file
  file = $fopen("bd_perf.rpt", "a");
  if (t_diff > 0) begin
    $fdisplay(file, "%m: Avg diff: %0t (%0d), Max diff: %0t (%.2f%%) @ %0t", tot_diff/n_A0, n_A0,  max_diff, 100.0*(max_diff-(tot_diff/n_A0))/(tot_diff/n_A0), t_diff);
  end
  $fclose(file);

end

endmodule


module bd_ct3_perf_mon (
  input logic \A[0] ,
  input logic \A[1] ,
  input logic \A[2] ,
  input logic X
);

logic idle;
always_comb idle = ( \A[0]  == \A[1]  && \A[1]  == \A[2]  && \A[2]  == \A[0] );

//Save the times of the last time signals transitioned
time t_A0, t_A1, t_A2;
int n_A0, n_A1, n_A2;
time max_diff;
time t_diff;
time tot_diff;


always @( \A[0] ) begin
  t_A0 = (X === 1'bx) ? '0 : $time;
  n_A0 = (X === 1'bx) ? '0 : n_A0+1;
end
always @( \A[1] ) begin
  t_A1 = (X === 1'bx) ? '0 : $time;
  n_A1 = (X === 1'bx) ? '0 : n_A1+1;
end
always @( \A[2] ) begin
  t_A2 = (X === 1'bx) ? '0 : $time;
  n_A2 = (X === 1'bx) ? '0 : n_A2+1;
end

time diff;
always @( X ) begin
  diff = `MAX3(t_A0, t_A1, t_A2) - `MIN3(t_A0, t_A1, t_A2);
  tot_diff += diff;
  if (max_diff < diff) begin
    max_diff = diff;
    t_diff = $time;
  end
end


integer file;
//Display results at the end of the simulation
final begin
  $timeformat(-12,0,"ps");
  if (idle == 1'b0) $display("%m: C-element inputs are not idenitical, last activity at %0t", `MAX3(t_A0, t_A1, t_A2));
  //open a new file
  file = $fopen("bd_perf.rpt", "a");
  if (t_diff > 0) begin
    $fdisplay(file, "%m: Avg diff: %0t (%0d), Max diff: %0t (%.2f%%) @ %0t", tot_diff/n_A0, n_A0, max_diff, 100.0*(max_diff-(tot_diff/n_A0))/(tot_diff/n_A0), t_diff);
  end
  $fclose(file);

end

endmodule

