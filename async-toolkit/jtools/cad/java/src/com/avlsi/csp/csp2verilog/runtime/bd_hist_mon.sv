`ifndef IMPORT_HIST_FUNCS
`define IMPORT_HIST_FUNCS
import "DPI-C" function bit histmon_register_node(string node_name);
import "DPI-C" function bit histmon_add_transition(string node_name, longint t1, string trigger_name, longint t2, string monitor_name);
import "DPI-C" function bit histmon_sim_done();
`endif //IMPORT_HIST_FUNCS

`timescale 1ps/1fs

`define MAX2(t1, t2) ((t1) > (t2) ? (t1) : (t2))
`define MAX3(t1, t2, t3) `MAX2((t1), `MAX2((t2), (t3)))

`define MIN2(t1, t2) ((t1) < (t2) ? (t1) : (t2))
`define MIN3(t1, t2, t3) `MIN2((t1), `MIN2((t2), (t3)))

//Module to log Controller history
module bd_ctrl_hist_mon (
  input logic rb,
  input logic \L.a ,
  input logic \L.q ,
  input logic \R.a ,
  input logic \R.q );

realtime t_Lq, t_Ra;
int n_Lq, n_Ra;
string ModName;

initial begin
  //Register ModName name
  t_Lq = '0;
  n_Lq = '0;
  t_Ra = '0;
  n_Ra = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".\\R.q "});
  histmon_register_node({ModName,".\\L.a "});
end

always @( \L.q ) begin
  t_Lq = (rb==1'b0) ? 1'b0 : $realtime;
  n_Lq = (rb==1'b0) ? '0 : n_Lq + 1;
end
always @( \R.a ) begin
  t_Ra = (rb==1'b0) ? 1'b0 : $realtime;
  n_Ra = (rb==1'b0) ? '0 : n_Ra + 1;
end

always @( \R.q ) begin
  if (t_Lq > t_Ra ) begin
    //L.q is the critical path
    //$display("%m: R.q fires @ %0t due to L.q @ %0t", $realtime, t_Lq);
    histmon_add_transition({ModName,".\\R.q "}, $realtime, {ModName,".\\L.q "}, t_Lq, "ctrl_mon");
  end
  else if (t_Ra > t_Lq ) begin 
    //R.a is the critical path
    //$display("%m: R.q fires @ %0t due to R.a @ %0t", $realtime, t_Ra);
    histmon_add_transition({ModName,".\\R.q "}, $realtime, {ModName,".\\R.a "}, t_Ra, "ctrl_mon");
  end
  else begin
    $display("%m: R.q fires @ %0t due to BOTH L.q and R.a @ %0t", $realtime, t_Lq);
    //Picking L.q
    histmon_add_transition({ModName,".\\R.q "}, $realtime, {ModName,".\\L.q "}, t_Lq, "ctrl_mon");
  end
end

always @( \L.a ) begin
  if (t_Lq > t_Ra ) begin
    //L.q is the critical path
    //$display("%m: L.a fires @ %0t due to L.q @ %0t", $realtime, t_Lq);
    histmon_add_transition({ModName,".\\L.a "}, $realtime, {ModName,".\\L.q "}, t_Lq, "ctrl_mon");
  end
  else if (t_Ra > t_Lq ) begin 
    //R.a is the critical path
    //$display("%m: L.a fires @ %0t due to R.a @ %0t", $realtime, t_Ra);
    histmon_add_transition({ModName,".\\L.a "}, $realtime, {ModName,".\\R.a "}, t_Ra, "ctrl_mon");
  end
  else begin
    $display("%m: L.a fires @ %0t due to BOTH L.q and R.a @ %0t", $realtime, t_Lq);
    //Picking L.q
    histmon_add_transition({ModName,".\\L.a "}, $realtime, {ModName,".\\L.q "}, t_Lq, "ctrl_mon");    
  end
end

final begin
  histmon_sim_done();
end

endmodule


//Module to log ctree2 history
module bd_ct2_hist_mon (
  input logic \A[0] ,
  input logic \A[1] ,
  input logic X
);

realtime t_A0, t_A1;
int n_A0, n_A1;
string ModName;
logic [1:0] saved, changed;

initial begin
  //Register ModName name
  t_A0 = '0;
  n_A0 = '0;
  t_A1 = '0;
  n_A1 = '0;
  saved = '0;
  changed = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".X"});
end

always @( \A[0] ) begin
  t_A0 = (X === 1'bx) ? '0 : $realtime;
  n_A0 = (X === 1'bx) ? '0 : n_A0+1;
end
always @( \A[1] ) begin
  t_A1 = (X === 1'bx) ? '0 : $realtime;
  n_A1 = (X === 1'bx) ? '0 : n_A1+1;
end

always @( \A[0]  or \A[1] ) begin
  if (\A[0]  !== 1'bx && \A[1]  !== 1'bx) begin
    changed <= saved ^ {\A[1] ,\A[0] };
    saved <= {\A[1] ,\A[0] };
  end
end

always @( X ) begin
  if (changed == (1'b1 << 0)) begin
    //A0 is the critical path
    //$display("%m: X fires @ %0t due to A[0] @ %0t", $realtime, t_A0);
    histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[0] "}, t_A0, "ct2_mon");
  end
  else if (changed == (1'b1 << 1)) begin
    //A1 is the critical path
    //$display("%m: X fires @ %0t due to A[1] @ %0t", $realtime, t_A1);
    histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[1] "}, t_A1, "ct2_mon");
  end
  else begin
    $display("%m: X fires @ %0t because changed = %2b", $realtime, changed);
    if (changed[0] == 1'b1) begin
      //A0 is one possible critical path
      //$display("%m: X fires @ %0t... picking A[0] @ %0t", $realtime, t_A0);
      histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[0] "}, t_A0, "ct2_mon");
    end
    else if (changed[1] == 1'b1) begin
      //A1 is one possible critical path
      //$display("%m: X fires @ %0t... picking A[1] @ %0t", $realtime, t_A1);
      histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[1] "}, t_A1, "ct2_mon");
    end
    else begin
      $display("%m: ERROR: X fires @ %0t", $realtime);
    end
  end
end

final begin
  histmon_sim_done();
end

endmodule

//Module to log ctree3 history
module bd_ct3_hist_mon (
  input logic \A[0] ,
  input logic \A[1] ,
  input logic \A[2] ,
  input logic X );

realtime t_A0, t_A1, t_A2;
int n_A0, n_A1, n_A2;
string ModName;
logic [2:0] saved, changed;

initial begin
  //Register ModName name
  t_A0 = '0;
  t_A1 = '0;
  t_A2 = '0;
  n_A0 = '0;
  n_A1 = '0;
  n_A2 = '0;
  saved = '0;
  changed = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".X"});
end

always @( \A[0] ) begin
  t_A0 = (X === 1'bx) ? 1'b0 : $realtime;
  n_A0 = (X === 1'bx) ? '0 : n_A0+1;
end
always @( \A[1] ) begin
  t_A1 = (X === 1'bx) ? 1'b0 : $realtime;
  n_A1 = (X === 1'bx) ? '0 : n_A1+1;
end
always @( \A[2] ) begin
  t_A2 = (X === 1'bx) ? 1'b0 : $realtime;
  n_A2 = (X === 1'bx) ? '0 : n_A2+1;
end

always @(\A[0]  or \A[1]  or \A[2] ) begin
  if (\A[0]  !== 1'bx && \A[1]  !== 1'bx && \A[2]  !== 1'bx) begin
    changed <= saved ^ {\A[2] ,\A[1] ,\A[0] };
    saved <= {\A[2] ,\A[1] ,\A[0] };
  end
end


always @( X ) begin
  if (changed == (1'b1 << 0)) begin
    //A0 is the critical input
    //$display("%m: X fires @ %0t due to A[0] @ %0t", $realtime, t_A0);
    histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[0] "}, t_A0, "ct3_mon");
  end
  else if (changed == (1'b1 << 1)) begin
    //A1 is the critical input
    //$display("%m: X fires @ %0t due to A[1] @ %0t", $realtime, t_A1);
    histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[1] "}, t_A1, "ct3_mon");
  end
  else if (changed == (1'b1 << 2)) begin
    //A2 is the critical input
    //$display("%m: X fires @ %0t due to A[2] @ %0t", $realtime, t_A2);
    histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[2] "}, t_A2, "ct3_mon");
  end
  else begin
    $display("%m: X fires @ %0t because changed = %3b", $realtime, changed);
    if (changed[0] == 1'b1) begin
      //A0 is one possible critical path
      //$display("%m: X fires @ %0t... picking A[0] @ %0t", $realtime, t_A0);
      histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[0] "}, t_A0, "ct3_mon");
    end
    else if (changed[1] == 1'b1) begin
      //A1 is one possible critical path
      //$display("%m: X fires @ %0t... picking A[1] @ %0t", $realtime, t_A1);
      histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[1] "}, t_A1, "ct3_mon");
    end
    else if (changed[2] == 1'b1) begin
      //A2 is one possible critical path
      //$display("%m: X fires @ %0t... picking A[2] @ %0t", $realtime, t_A2);
      histmon_add_transition({ModName,".X"}, $realtime, {ModName,".\\A[2] "}, t_A2, "ct3_mon");
    end
    else begin
      $display("%m: ERROR: X fires @ %0t", $realtime);
    end

  end
end

final begin
  histmon_sim_done();
end

endmodule

//Altverter/Diverter
module bd_altdiv_hist_mon (
  input logic rb,
  input logic L,
  input logic \R[0] ,
  input logic \R[1] 
);

realtime t_L;
int n_L;
string ModName;

initial begin
  //Register ModName name
  t_L = '0;
  n_L = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".L"});
end

always @(L) begin
  t_L = (rb==1'b0) ? 1'b0 : $realtime;
  n_L = (rb==1'b0) ? '0 : n_L + 1;
end

always @( \R[0]  ) begin
    //L is (always) the critical path
    //$display("%m: R[0] fires @ %0t due to L @ %0t", $realtime, t_L);
    histmon_add_transition({ModName,".\\R[0] "}, $realtime, {ModName,".L"}, t_L, "altdiv_mon");
end

always @( \R[1]  ) begin
    //L is (always) the critical path
    //$display("%m: R[1] fires @ %0t due to L @ %0t", $realtime, t_L);
    histmon_add_transition({ModName,".\\R[1] "}, $realtime, {ModName,".L"}, t_L, "altdiv_mon");
end

final begin
  histmon_sim_done();
end

endmodule


//Slackless arbiter
module bd_szarb2_hist_mon (
  input logic \L[0].0 ,
  input logic \L[0].e ,
  input logic \L[1].0 ,
  input logic \L[1].e ,
  input logic \R.0 ,
  input logic \R.e );

realtime t_L0, t_L1, t_Re;
int n_L0, n_L1, n_Re;
string ModName;

initial begin
  //Register ModName name
  t_L0 = '0;
  t_L1 = '0;
  t_Re = '0;
  n_L0 = '0;
  n_L1 = '0;
  n_Re = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".\\R.0 "});
  histmon_register_node({ModName,".\\L[0].e "});
  histmon_register_node({ModName,".\\L[1].e "});
end

always @( \L[0].0 ) begin 
  t_L0 = (\R.0 === 1'bx) ? 1'b0 : $realtime;
  n_L0 = (\R.0 === 1'bx) ? 1'b0 : n_L0+1;
end
always @( \L[1].0 ) begin
  t_L1 = (\R.0 === 1'bx) ? 1'b0 : $realtime;
  n_L1 = (\R.0 === 1'bx) ? 1'b0 : n_L1+1;
end
always @( \R.e ) begin
  t_Re = (\R.0 === 1'bx) ? 1'b0 : $realtime;
  n_Re = (\R.0 === 1'bx) ? 1'b0 : n_Re+1;
end

always @( \L[0].e ) begin
  if (t_L0 > t_Re) begin
    //$display("%m: L[0].e fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\L[0].e "}, $realtime, {ModName,".\\L[0].0 "}, t_L0, "slarb2_mon");
  end
  else if (t_Re > t_L0) begin
    //$display("%m: L[0].e fires @ %0t due to R.e @ %0t", $realtime, t_Re);
    histmon_add_transition({ModName,".\\L[0].e "}, $realtime, {ModName,".\\R.e "}, t_Re, "slarb2_mon");
  end
  else begin
    $display("%m: L[0].e fires @ %0t due to BOTH L[0] and R.e @ %0t", $realtime, t_L0);
  end
end

always @( \L[1].e ) begin
  if (t_L1 > t_Re) begin
    //$display("%m: L[1].e fires @ %0t due to L[1] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\L[1].e "}, $realtime, {ModName,".\\L[1].0 "}, t_L1, "slarb2_mon");
  end
  else if (t_Re > t_L1) begin
    //$display("%m: L[1].e fires @ %0t due to R.e @ %0t", $realtime, t_Re);
    histmon_add_transition({ModName,".\\L[1].e "}, $realtime, {ModName,".\\R.e "}, t_Re, "slarb2_mon");
  end
  else begin
    $display("%m: L[1].e fires @ %0t due to BOTH L[1] and R.e @ %0t", $realtime, t_L1);
  end
end

always @( \R.0 ) begin
  if (t_L0 > t_L1 && t_L0 > t_Re) begin
    //$display("%m: R.0 fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\R.0 "}, $realtime, {ModName,".\\L[0].0 "}, t_L0, "slarb2_mon");
  end
  else if (t_L1 > t_L0 && t_L1 > t_Re) begin
    //$display("%m: R.0 fires @ %0t due to L[1] @ %0t", $realtime, t_L1);
    histmon_add_transition({ModName,".\\R.0 "}, $realtime, {ModName,".\\L[1].0 "}, t_L1, "slarb2_mon");
  end
  else if (t_Re > t_L0 && t_Re > t_L1) begin
    //$display("%m: R.0 fires @ %0t due to R.e @ %0t", $realtime, t_Re);
    histmon_add_transition({ModName,".\\R.0 "}, $realtime, {ModName,".\\R.e "}, t_Re, "slarb2_mon");
  end
  else begin
    //All 3?!?! 
  end
end

final begin
  histmon_sim_done();
end

endmodule

//Arbiter (pipelined)
module bd_arb2_hist_mon (
  input logic rb,
  input logic \L[0].0 ,
  input logic \L[0].e ,
  input logic \L[1].0 ,
  input logic \L[1].e ,
  input logic \A.0 ,
  input logic \A.1 ,
  input logic \A.e );

realtime t_L0, t_L1, t_Ae;
int n_L0, n_L1, n_Ae;
string ModName;

initial begin
  //Register ModName name
  t_L0 = '0;
  t_L1 = '0;
  t_Ae = '0;
  n_L0 = '0;
  n_L1 = '0;
  n_Ae = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".\\A.0 "});
  histmon_register_node({ModName,".\\A.1 "});
  histmon_register_node({ModName,".\\L[0].e "});
  histmon_register_node({ModName,".\\L[1].e "});
end

always @( \L[0].0 ) begin 
  t_L0 = (rb==1'b0) ? 1'b0 : $realtime;
  n_L0 = (rb==1'b0) ? 1'b0 : n_L0+1;
end
always @( \L[1].0 ) begin
  t_L1 = (rb==1'b0) ? 1'b0 : $realtime;
  n_L1 = (rb==1'b0) ? 1'b0 : n_L1+1;
end
always @( \A.e ) begin
  t_Ae = (rb==1'b0) ? 1'b0 : $realtime;
  n_Ae = (rb==1'b0) ? 1'b0 : n_Ae+1;
end

always @( \L[0].e ) begin
  if (t_L0 > t_Ae) begin
    //$display("%m: L[0].e fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\L[0].e "}, $realtime, {ModName,".\\L[0].0 "}, t_L0, "arb2_mon");
  end
  else if (t_Ae > t_L0) begin
    //$display("%m: L[0].e fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\L[0].e "}, $realtime, {ModName,".\\A.e "}, t_Ae, "arb2_mon");
  end
  else begin
    $display("%m: L[0].e fires @ %0t due to BOTH L[0] and A.e @ %0t", $realtime, t_L0);
  end
end

always @( \L[1].e ) begin
  if (t_L1 > t_Ae) begin
    //$display("%m: L[1].e fires @ %0t due to L[1] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\L[1].e "}, $realtime, {ModName,".\\L[1].0 "}, t_L1, "arb2_mon");
  end
  else if (t_Ae > t_L1) begin
    //$display("%m: L[1].e fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\L[1].e "}, $realtime, {ModName,".\\A.e "}, t_Ae, "arb2_mon");
  end
  else begin
    $display("%m: L[1].e fires @ %0t due to BOTH L[1] and A.e @ %0t", $realtime, t_L1);
  end
end

always @( \A.0 ) begin
  if (t_L0 > t_Ae) begin
    //$display("%m: A.0 fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\A.0 "}, $realtime, {ModName,".\\L[0].0 "}, t_L0, "arb2_mon");
  end
  else if (t_Ae > t_L0) begin
    //$display("%m: A.0 fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\A.0 "}, $realtime, {ModName,".\\A.e "}, t_Ae, "arb2_mon");
  end
  else begin
    $display("%m: A.0 fires @ %0t due to BOTH L[0] and A.e @ %0t", $realtime, t_L0);
  end
end

always @( \A.1 ) begin
  if (t_L1 > t_Ae) begin
    //$display("%m: A.1 fires @ %0t due to L[1] @ %0t", $realtime, t_L1);
    histmon_add_transition({ModName,".\\A.1 "}, $realtime, {ModName,".\\L[1].0 "}, t_L1, "arb2_mon");
  end
  else if (t_Ae > t_L1) begin
    //$display("%m: A.1 fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\A.1 "}, $realtime, {ModName,".\\A.e "}, t_Ae, "arb2_mon");
  end
  else begin
    $display("%m: A.1 fires @ %0t due to BOTH L[1] and A.e @ %0t", $realtime, t_L1);
  end
end

final begin
  histmon_sim_done();
end

endmodule

module bd_probe_hist_mon (
  input logic rb ,
  input logic \A.0 ,
  input logic \A.1 ,
  input logic \A.e ,
  input logic \L.0 ,
  input logic \L.e ,
  input logic \Q.0 ,
  input logic \Q.1 ,
  input logic \Q.e );

realtime t_Ae, t_L0, t_Q0, t_Q1;
int n_Ae, n_L0, n_Q0, n_Q1;
string ModName;

initial begin
  //Register ModName name
  t_Ae = '0;
  t_L0 = '0;
  t_Q0 = '0;
  t_Q1 = '0;
  n_Ae = '0;
  n_L0 = '0;
  n_Q0 = '0;
  n_Q1 = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".\\A.0 "});
  histmon_register_node({ModName,".\\A.1 "});
  histmon_register_node({ModName,".\\L.e "});
  histmon_register_node({ModName,".\\Q.e "});
end

always @( \A.e ) begin
  t_Ae = (rb==1'b0) ? 1'b0 : $realtime;
  n_Ae = (rb==1'b0) ? 1'b0 : n_Ae+1;
end
always @( \L.0 ) begin
  t_L0 = (rb==1'b0) ? 1'b0 : $realtime;
  n_L0 = (rb==1'b0) ? 1'b0 : n_L0+1;
end
always @( \Q.0 ) begin
  t_Q0 = (rb==1'b0) ? 1'b0 : $realtime;
  n_Q0 = (rb==1'b0) ? 1'b0 : n_Q0+1;
end
always @( \Q.1 ) begin
  t_Q1 = (rb==1'b0) ? 1'b0 : $realtime;
  n_Q1 = (rb==1'b0) ? 1'b0 : n_Q1+1;
end

always @( \A.0 ) begin
  if (t_L0 > t_Ae && t_L0 > t_Q0 && t_L0 > t_Q1) begin
    //$display("%m: A.0 fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\A.0 "}, $realtime, {ModName,".\\L.0 "}, t_L0, "probe_mon");
  end
  else if (t_Ae > t_L0 && t_Ae > t_Q0 && t_Ae > t_Q1) begin
    //$display("%m: A.0 fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\A.0 "}, $realtime, {ModName,".\\A.e "}, t_Ae, "probe_mon");
  end
  else if (t_Q0 > t_L0 && t_Q0 > t_Ae && t_Q0 > t_Q1) begin
    //$display("%m: A.0 fires @ %0t due to Q[0] @ %0t", $realtime, t_Q0);
    histmon_add_transition({ModName,".\\A.0 "}, $realtime, {ModName,".\\Q.0 "}, t_Q0, "probe_mon");
  end
  else if (t_Q1 > t_L0 && t_Q1 > t_Ae && t_Q1 > t_Q0) begin
    //$display("%m: A.0 fires @ %0t due to Q[1] @ %0t", $realtime, t_Q1);
    histmon_add_transition({ModName,".\\A.0 "}, $realtime, {ModName,".\\Q.1 "}, t_Q1, "probe_mon");
  end
  else begin
    $display("%m: A.0 fires @ %0t but could not determine causality", $realtime);
  end
end

always @( \A.1 ) begin
  if (t_L0 > t_Ae && t_L0 > t_Q0 && t_L0 > t_Q1) begin
    //$display("%m: A.1 fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\A.1 "}, $realtime, {ModName,".\\L.0 "}, t_L0, "probe_mon");
  end
  else if (t_Ae > t_L0 && t_Ae > t_Q0 && t_Ae > t_Q1) begin
    //$display("%m: A.1 fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\A.1 "}, $realtime, {ModName,".\\A.e "}, t_Ae, "probe_mon");
  end
  else if (t_Q0 > t_L0 && t_Q0 > t_Ae && t_Q0 > t_Q1) begin
    //$display("%m: A.1 fires @ %0t due to Q[0] @ %0t", $realtime, t_Q0);
    histmon_add_transition({ModName,".\\A.1 "}, $realtime, {ModName,".\\Q.0 "}, t_Q0, "probe_mon");
  end
  else if (t_Q1 > t_L0 && t_Q1 > t_Ae && t_Q1 > t_Q0) begin
    //$display("%m: A.1 fires @ %0t due to Q.1 @ %0t", $realtime, t_Q1);
    histmon_add_transition({ModName,".\\A.1 "}, $realtime, {ModName,".\\Q.1 "}, t_Q1, "probe_mon");
  end
  else begin
    $display("%m: A.1 fires @ %0t but could not determine causality", $realtime);
  end
end

always @( \L.e ) begin
  if (t_L0 > t_Ae && t_L0 > t_Q0 && t_L0 > t_Q1) begin
    //$display("%m: L.e fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\L.e "}, $realtime, {ModName,".\\L.0 "}, t_L0, "probe_mon");
  end
  else if (t_Ae > t_L0 && t_Ae > t_Q0 && t_Ae > t_Q1) begin
    //$display("%m: L.e fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\L.e "}, $realtime, {ModName,".\\A.e "}, t_Ae, "probe_mon");
  end
  else if (t_Q0 > t_L0 && t_Q0 > t_Ae && t_Q0 > t_Q1) begin
    //$display("%m: L.e fires @ %0t due to Q[0] @ %0t", $realtime, t_Q0);
    histmon_add_transition({ModName,".\\L.e "}, $realtime, {ModName,".\\Q.0 "}, t_Q0, "probe_mon");
  end
  else if (t_Q1 > t_L0 && t_Q1 > t_Ae && t_Q1 > t_Q0) begin
    //$display("%m: L.e fires @ %0t due to Q[1] @ %0t", $realtime, t_Q1);
    histmon_add_transition({ModName,".\\L.e "}, $realtime, {ModName,".\\Q.1 "}, t_Q1, "probe_mon");
  end
  else begin
    $display("%m: L.e fires @ %0t but could not determine causality", $realtime);
  end
end

always @( \Q.e ) begin
  if (t_L0 > t_Ae && t_L0 > t_Q0 && t_L0 > t_Q1) begin
    //$display("%m: Q.e fires @ %0t due to L[0] @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\Q.e "}, $realtime, {ModName,".\\L.0 "}, t_L0, "probe_mon");
  end
  else if (t_Ae > t_L0 && t_Ae > t_Q0 && t_Ae > t_Q1) begin
    //$display("%m: Q.e fires @ %0t due to A.e @ %0t", $realtime, t_Ae);
    histmon_add_transition({ModName,".\\Q.e "}, $realtime, {ModName,".\\A.e "}, t_Ae, "probe_mon");
  end
  else if (t_Q0 > t_L0 && t_Q0 > t_Ae && t_Q0 > t_Q1) begin
    //$display("%m: Q.e fires @ %0t due to Q[0] @ %0t", $realtime, t_Q0);
    histmon_add_transition({ModName,".\\Q.e "}, $realtime, {ModName,".\\Q.0 "}, t_Q0, "probe_mon");
  end
  else if (t_Q1 > t_L0 && t_Q1 > t_Ae && t_Q1 > t_Q0) begin
    //$display("%m: Q.e fires @ %0t due to Q[1] @ %0t", $realtime, t_Q1);  
    histmon_add_transition({ModName,".\\Q.e "}, $realtime, {ModName,".\\Q.1 "}, t_Q1, "probe_mon");
  end
  else begin
    $display("%m: Q.e fires @ %0t but could not determine causality", $realtime);
  end
end

final begin
  histmon_sim_done();
end

endmodule

module bd_buf1of2_hist_mon (
  input logic rb ,
  input logic \L.0 ,
  input logic \L.1 ,
  input logic \L.e ,
  input logic \R.0 ,
  input logic \R.1 ,
  input logic \R.e );

realtime t_L0, t_L1, t_Re;
int n_L0, n_L1, n_Re;
string ModName;

initial begin
  //Register ModName name
  t_L0 = '0;
  t_L1 = '0;
  t_Re = '0;
  n_L0 = '0;
  n_L1 = '0;
  n_Re = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".\\R.0 "});
  histmon_register_node({ModName,".\\R.1 "});
  histmon_register_node({ModName,".\\L.e "});
end

always @( \L.0 ) begin 
  t_L0 = (rb==1'b0) ? 1'b0 : $realtime;
  n_L0 = (rb==1'b0) ? 1'b0 : n_L0+1;
end
always @( \L.1 ) begin
  t_L1 = (rb==1'b0) ? 1'b0 : $realtime;
  n_L1 = (rb==1'b0) ? 1'b0 : n_L1+1;
end
always @( \R.e ) begin
  t_Re = (rb==1'b0) ? 1'b0 : $realtime;
  n_Re = (rb==1'b0) ? 1'b0 : n_Re+1;
end

always @( \R.0 ) begin
  if (t_L0 > t_Re ) begin
    //L.0 is the critical path
    //$display("%m: R.0 fires @ %0t due to L.0 @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\R.0 "}, $realtime, {ModName,".\\L.0 "}, t_L0, "buf_mon");
  end
  else if (t_Re > t_L0) begin
    //R.e is the critical path
    //$display("%m: R.0 fires @ %0t due to R.e @ %0t", $realtime, t_Re);
    histmon_add_transition({ModName,".\\R.0 "}, $realtime, {ModName,".\\R.e "}, t_Re, "buf_mon");
  end
  else begin
    $display("%m: R.0 fires @ %0t due to BOTH L.0 and R.e @ %0t", $realtime, t_L0);
  end
end

always @( \R.1 ) begin
  if (t_L1 > t_Re ) begin
    //L.1 is the critical path
    //$display("%m: R.1 fires @ %0t due to L.1 @ %0t", $realtime, t_L1);
    histmon_add_transition({ModName,".\\R.1 "}, $realtime, {ModName,".\\L.1 "}, t_L1, "buf_mon");
  end
  else if (t_Re > t_L1) begin
    //R.e is the critical path
    //$display("%m: R.1 fires @ %0t due to R.e @ %0t", $realtime, t_Re);
    histmon_add_transition({ModName,".\\R.1 "}, $realtime, {ModName,".\\R.e "}, t_Re, "buf_mon");
  end
  else begin
    $display("%m: R.1 fires @ %0t due to BOTH L.1 and R.e @ %0t", $realtime, t_11);
  end
end

always @( \L.e ) begin
  if (t_L0 > t_L1 ) begin
    //L.0 is the critical path
    //$display("%m: L.e fires @ %0t due to L.0 @ %0t", $realtime, t_L0);
    histmon_add_transition({ModName,".\\L.e "}, $realtime, {ModName,".\\L.0 "}, t_L0, "buf_mon");
  end
  else if (t_L1 > t_L0) begin
    //L.1 is the critical path
    //$display("%m: L.e fires @ %0t due to R.e @ %0t", $realtime, t_Re);
    histmon_add_transition({ModName,".\\L.e "}, $realtime, {ModName,".\\L.1 "}, t_L1, "buf_mon");
  end
  else begin
    $display("%m: Error, L.0 and L.1 should be exclusive");
  end
end

final begin
  histmon_sim_done();
end

endmodule

//Generic 2-input combinational gate
module bd_comb2_hist_mon #(string outname="out") (
  input logic a,
  input logic b,
  input logic out );

realtime t_a, t_b;
int n_a, n_b;
string ModName;
logic [1:0] saved, changed;

initial begin
  t_a = '0;
  t_b = '0;
  n_a = '0;
  n_b = '0;
  saved = '0;
  changed = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".",outname});
end

always @(a) begin
  t_a = (out === 1'bx) ? 1'b0 : $realtime;
  n_a = (out === 1'bx) ? '0 : n_a+1;
end
always @(b) begin
  t_b = (out === 1'bx) ? 1'b0 : $realtime;
  n_b = (out === 1'bx) ? '0 : n_b+1;
end

always @(a or b) begin
  if (a !== 1'bx && b !== 1'bx) begin
    changed <= saved ^ {b,a};
    saved <= {b,a};
  end
end

always @(out) begin
  if (changed == (1'b1 << 0)) begin
    //a is the critical path
    //$display("%m: out fires @ %0t due to a @ %0t", $realtime, t_a);
    histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".a"}, t_a, "comb2_mon");
  end
  else if (changed == (1'b1 << 1)) begin
    //b is the critical path
    //$display("%m: out fires @ %0t due to b @ %0t", $realtime, t_b);
    histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".b"}, t_b, "comb2_mon");
  end
  else begin
    $display("%m: out fires @ %0t because changed = %2b", $realtime, changed);
    if (changed[0] == 1'b1) begin
      //a is one possible critical path
      //$display("%m: out fires @ %0t... picking a @ %0t", $realtime, t_a);
      histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".a"}, t_a, "comb2_mon");
    end
    else if (changed[1] == 1'b1) begin
      //b is one possible critical path
      //$display("%m: out fires @ %0t... picking b @ %0t", $realtime, t_b);
      histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".b"}, t_b, "comb2_mon");
    end
    else begin
      $display("%m: ERROR: out fires @ %0t", $realtime);
    end
  end
end

final begin
  histmon_sim_done();
end

endmodule

//Generic 3-input combinational gate
module bd_comb3_hist_mon #(string outname="out") (
  input logic a,
  input logic b,
  input logic c,
  input logic out );

realtime t_a, t_b, t_c;
int n_a, n_b, n_c;
string ModName;
logic [2:0] saved, changed;

initial begin
  t_a = '0;
  t_b = '0;
  t_c = '0;
  n_a = '0;
  n_b = '0;
  n_c = '0;
  saved = '0;
  changed = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".",outname});
end

always @(a) begin
  t_a = (out === 1'bx) ? 1'b0 : $realtime;
  n_a = (out === 1'bx) ? '0 : n_a+1;
end
always @(b) begin
  t_b = (out === 1'bx) ? 1'b0 : $realtime;
  n_b = (out === 1'bx) ? '0 : n_b+1;
end
always @(c) begin
  t_c = (out === 1'bx) ? 1'b0 : $realtime;
  n_c = (out === 1'bx) ? '0 : n_c+1;
end

always @(a or b or c) begin
  if (a !== 1'bx && b !== 1'bx && c !== 1'bx) begin
    changed <= saved ^ {c,b,a};
    saved <= {c,b,a};
  end
end

always @(out) begin
  if (changed == (1'b1 << 0)) begin
    //a is the critical path
    //$display("%m: out fires @ %0t due to a @ %0t", $realtime, t_a);
    histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".a"}, t_a, "comb3_mon");
  end
  else if (changed == (1'b1 << 1)) begin
    //b is the critical path
    //$display("%m: out fires @ %0t due to b @ %0t", $realtime, t_b);
    histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".b"}, t_b, "comb3_mon");
  end
  else if (changed == (1'b1 << 2)) begin
    //c is the critical path
    //$display("%m: out fires @ %0t due to c @ %0t", $realtime, t_c);
    histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".c"}, t_c, "comb3_mon");
  end
  else begin
    $display("%m: out fires @ %0t because changed = %3b", $realtime, changed);
    if (changed[0] == 1'b1) begin
      //a is one possible critical path
      //$display("%m: out fires @ %0t... picking a @ %0t", $realtime, t_a);
      histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".a"}, t_a, "comb3_mon");
    end
    else if (changed[1] == 1'b1) begin
      //b is one possible critical path
      //$display("%m: out fires @ %0t... picking b @ %0t", $realtime, t_b);
      histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".b"}, t_b, "comb3_mon");
    end
    else if (changed[2] == 1'b1) begin
      //c is one possible critical path
      //$display("%m: out fires @ %0t... picking c @ %0t", $realtime, t_c);
      histmon_add_transition({ModName,".",outname}, $realtime, {ModName,".c"}, t_c, "comb3_mon");
    end
    else begin
      $display("%m: ERROR: out fires @ %0t", $realtime);
    end
  end
end

final begin
  histmon_sim_done();
end

endmodule

module bd_cg_hist_mon (
  input logic clk,
  input logic en,
  input logic clkout );

realtime t_clk, t_en;
int n_clk, n_en;
string ModName;

initial begin
  t_clk = '0;
  t_en = '0;
  n_clk = '0;
  n_en = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,".clkout"});
end

always @(clk) begin
  t_clk = (clkout === 1'bx) ? 1'b0 : $realtime;
  n_clk = (clkout === 1'bx) ? '0 : n_clk+1;
end
always @(en) begin
  t_en = (clkout === 1'bx) ? 1'b0 : $realtime;
  n_en = (clkout === 1'bx) ? '0 : n_en+1;
end

always @(clkout) begin
  if (t_clk > t_en) begin
    //a is the critical path
    //$display("%m: out fires @ %0t due to clk @ %0t", $realtime, t_a);
    histmon_add_transition({ModName,".clkout"}, $realtime, {ModName,".clk"}, t_clk, "cg_mon");
  end
  else if (t_en > t_clk) begin
    //b is the critical path
    //$display("%m: out fires @ %0t due to en @ %0t", $realtime, t_b);
    histmon_add_transition({ModName,".clkout"}, $realtime, {ModName,".en"}, t_en, "cg_mon");
  end
  else begin
    $display("%m: clkout fires @ %0t due to BOTH a and b @ %0t", $realtime, t_clk);
  end
end

final begin
  histmon_sim_done();
end

endmodule


module bd_ntvsram_hist_mon (
  input logic \IA.C.a ,
  input logic \IA.C.q ,
  input logic \R.C.a ,
  input logic \R.C.q ,
  input logic \W.C.a ,
  input logic \W.C.q );

realtime t_IA_C_q, t_R_C_a, t_W_C_q;
int n_IA_C_q, n_R_C_a, n_W_C_q;
string ModName;

initial begin
  t_IA_C_q = '0;
  t_R_C_a = '0;
  t_W_C_q = '0;
  n_IA_C_q = '0;
  n_R_C_a = '0;
  n_W_C_q = '0;
  ModName = $sformatf("%m");
  $timeformat(-15,0,"fs");
  histmon_register_node({ModName,"\\IA..a"});
  histmon_register_node({ModName,"\\W.C.a"});
  histmon_register_node({ModName,"\\R.C.q"});
end

always @( \IA.C.q ) begin
  t_IA_C_q = ( \IA.C.a === 1'bx) ? 1'b0 : $realtime;
  n_IA_C_q = ( \IA.C.a === 1'bx) ? '0 : n_IA_C_q+1;
end
always @( \R.C.a ) begin
  t_R_C_a = ( \R.C.q === 1'bx) ? 1'b0 : $realtime;
  n_R_C_a = ( \R.C.q === 1'bx) ? '0 : n_R_C_a+1;
end
always @( \W.C.q ) begin
  t_W_C_q = ( \W.C.a === 1'bx) ? 1'b0 : $realtime;
  n_W_C_q = ( \W.C.a === 1'bx) ? '0 : n_W_C_q+1;
end

always @( \IA.C.a ) begin
  if (t_IA_C_q > t_R_C_a && t_IA_C_q > t_W_C_q) begin
    //IA.C.q is the critical path
    //$display("%m: IA.C.a fires @ %0t due to IA.C.q @ %0t", $realtime, t_IA_C_q);
    histmon_add_transition({ModName,"\\IA.C.a"}, $realtime, {ModName,"\\IA.C.q"}, t_IA_C_q, "ntvsram_mon");
  end
  else if (t_R_C_a > t_IA_C_q && t_R_C_a > t_W_C_q) begin
    //R.C.a is the critical path
    //$display("%m: IA.C.a fires @ %0t due to R.C.a @ %0t", $realtime, t_R_C_a);
    histmon_add_transition({ModName,"\\IA.C.a"}, $realtime, {ModName,"\\R.C.a"}, t_R_C_a, "ntvsram_mon");
  end
  else if (t_W_C_q > t_IA_C_q && t_W_C_q > t_R_C_a) begin
    //W.C.q is the critical path
    //$display("%m: IA.C.a fires @ %0t due to W.C.q @ %0t", $realtime, t_W_C_q);
    histmon_add_transition({ModName,"\\IA.C.a"}, $realtime, {ModName,"\\W.C.q"}, t_W_C_q, "ntvsram_mon");
  end
  else begin
    $display("%m: IA.C.a fires @ %0t but could not determine causality", $realtime);
  end
end

always @( \R.C.q ) begin
  if (t_IA_C_q > t_R_C_a && t_IA_C_q > t_W_C_q) begin
    //IA.C.q is the critical path
    //$display("%m: R.C.q fires @ %0t due to IA.C.q @ %0t", $realtime, t_IA_C_q);
    histmon_add_transition({ModName,"\\R.C.q"}, $realtime, {ModName,"\\IA.C.q"}, t_IA_C_q, "ntvsram_mon");
  end
  else if (t_R_C_a > t_IA_C_q && t_R_C_a > t_W_C_q) begin
    //R.C.a is the critical path    
    //$display("%m: R.C.q fires @ %0t due to R.C.a @ %0t", $realtime, t_R_C_a);
    histmon_add_transition({ModName,"\\R.C.q"}, $realtime, {ModName,"\\R.C.a"}, t_R_C_a, "ntvsram_mon");
  end
  else if (t_W_C_q > t_IA_C_q && t_W_C_q > t_R_C_a) begin
    //W.C.q is the critical path
    //$display("%m: R.C.q fires @ %0t due to W.C.q @ %0t", $realtime, t_W_C_q);
    histmon_add_transition({ModName,"\\R.C.q"}, $realtime, {ModName,"\\W.C.q"}, t_W_C_q, "ntvsram_mon");
  end
  else begin
    $display("%m: R.C.q fires @ %0t but could not determine causality", $realtime);
  end
end

always @( \W.C.a ) begin
  if (t_IA_C_q > t_R_C_a && t_IA_C_q > t_W_C_q) begin
    //IA.C.q is the critical path
    //$display("%m: W.C.a fires @ %0t due to IA.C.q @ %0t", $realtime, t_IA_C_q);
    histmon_add_transition({ModName,"\\W.C.a"}, $realtime, {ModName,"\\IA.C.q"}, t_IA_C_q, "ntvsram_mon");
  end
  else if (t_R_C_a > t_IA_C_q && t_R_C_a > t_W_C_q) begin
    //R.C.a is the critical path
    //$display("%m: W.C.a fires @ %0t due to R.C.a @ %0t", $realtime, t_R_C_a);
    histmon_add_transition({ModName,"\\W.C.a"}, $realtime, {ModName,"\\R.C.a"}, t_R_C_a, "ntvsram_mon");
  end
  else if (t_W_C_q > t_IA_C_q && t_W_C_q > t_R_C_a) begin
    //W.C.q is the critical path
    //$display("%m: W.C.a fires @ %0t due to W.C.q @ %0t", $realtime, t_W_C_q);
    histmon_add_transition({ModName,"\\W.C.a"}, $realtime, {ModName,"\\W.C.q"}, t_W_C_q, "ntvsram_mon");
  end
  else begin
    $display("%m: W.C.a fires @ %0t but could not determine causality", $realtime);
  end
end

final begin
  histmon_sim_done();
end

endmodule
