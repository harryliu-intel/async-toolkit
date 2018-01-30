
//
// Vernier Timestamping 
//
// Author : Mika Nystrom <mika.nystroem@intel.com>
// May, 2015
//

// careful here: 
//
// the reset strategy we have to be careful about.  we should probably
// make the B side reset off i_vernier_start, NOT rst_n.  There is no
// particular reason to reset on rst_n, except some sort of sanity.
// The time reset of the B side is necessary is on i_vernier_start!

module sync2xdff0_x2 (
  input  logic  ck, d, se, si,
  output logic  q );

  // model a depth-2 synchronizer

  logic qq;

  always_ff @(posedge ck) begin
    qq <= d;
    q  <= qq;
  end
endmodule

module tsucnt 
  #(parameter FCLK_DIV_BITS= 3,
    parameter FCLK_RST_BITS=16,
    parameter RAT_PREC_BITS=32
   )
   (
    input logic                      clk, // 1588 clock
    input logic                      rst_n, // 1588 reset

    // "B" synchronized inputs
    input logic                      i_fclk, // foreign clock

    // "A" synchronized control inputs
    input logic [FCLK_DIV_BITS-1:0]  i_fclk_div, // fgn clock div.
    input logic [FCLK_RST_BITS-1:0]  i_fclk_rst_cycs, // reset cycles
    
    input logic [RAT_PREC_BITS-1:0]  i_num, // 1588 period
    input logic [RAT_PREC_BITS-1:0]  i_denom, // fclk period
    
    
    output logic                     o_evt, // rising edge det
    output logic [FCLK_DIV_BITS-1:0] position, 
     
    output logic [RAT_PREC_BITS-1:0] o_phase
   );


  logic                   f_rst_n;
  logic [FCLK_DIV_BITS :0]      i_fclk_div_smaller;
  // must assert/check i_fclk_div == 1 or is even and != 0
  assign i_fclk_div_smaller = i_fclk_div - 1;

  // "B" clock position counter
  // XXX BUG : reset must be synchronized to B
  always_ff @(posedge i_fclk or negedge rst_n)
    begin
      if(!rst_n)
       position <= '0;
      else if(position < (i_fclk_div_smaller))
        position <= position + 1;
      else
        position <= '0;
    end

  logic div_clk;
  assign div_clk = (position < (i_fclk_div/2)) ? 1:0;

  logic sfclk;    
  assign sfclk = (i_fclk_div != 1) ? div_clk:i_fclk;

  sync2xdff0_x2 u_sync2xdff0_x2_sampler (
    .q   ( samp   ),
    .ck  ( clk    ), // 1588 clock
    .d   ( sfclk  ), // divided foreign clock
    .se  ( 1'b0   ),
    .si  ( 1'b0   )
  );

  logic psamp;

  always_ff @(posedge clk or negedge rst_n)
    psamp <= rst_n ? samp : '0;

  logic evt;

  assign evt = samp & ~psamp;
  
  always_ff @(posedge clk) o_evt <= evt;
  
  // a TRUE sample following a FALSE sample is a POSITIVE EDGE
  // we call that an EVENT.

  // the following logic tracks how long since the last event
  logic [FCLK_RST_BITS-1:0] evt_age_d, evt_age_q;

  assign evt_age_d = (~evt & rst_n) ? (evt_age_q + 1) : '0;

  always_ff @(posedge clk)
    evt_age_q <= evt_age_d;

  // if it has been a long time, assert f_rst_n for exactly one 1588 cycle
  // also assert it for as long as rst_n is asserted (is this right?)
  assign f_rst_n = ~(evt_age_q == i_fclk_rst_cycs) & rst_n;

  // we dont count 1588 cycles here

  // the phase counter
  
  logic [RAT_PREC_BITS-1:0] pcnt_d, pcnt_q, p_incr;

  assign p_incr = +i_num;

  always_comb 
    if (~f_rst_n)
      pcnt_d = '0;
    else if (evt)  
      pcnt_d = pcnt_q - i_denom * i_fclk_div + p_incr;  
     // Since the clock frequency has already been multiplied. The period should change
    else
      pcnt_d = pcnt_q                        + p_incr;

  always_ff @(posedge clk)
    pcnt_q <= pcnt_d;
  
  assign o_phase = pcnt_q;

endmodule

module tsu  
  #(parameter FCLK_DIV_BITS =  3,
    parameter FCLK_RST_BITS = 16, // XXX remove
    parameter RAT_PREC_BITS = 32,
    parameter SAMPLE_TIMES  =  2

    // debug parameters
    ,parameter DEBUG_GCD_UNITY = 0 // do not use GCD to divide periods
   )
   (
    input logic                      clk, // 1588 clock "A"
    input logic                      rst_n, // 1588 reset

    // "A" clock synchronized configuration inputs
    // N.B. all these inputs should be REGISTERED INTERNALLY in the final
    // implementation
    input logic [FCLK_DIV_BITS-1:0]  i_fclk_div, // foreign clock div.
    input logic [FCLK_RST_BITS-1:0]  i_fclk_rst_cycs, // reset cycles
    
    input logic [RAT_PREC_BITS-1:0]  i_num, // 1588 period
    input logic [RAT_PREC_BITS-1:0]  i_denom, // fclk period
    input logic [RAT_PREC_BITS-1:0]  i_denom_err, // fclk period error

    // "A" clock synchronized control handshake
    input logic                      i_vernier_start,
    output logic                     o_vernier_ready,
    output logic                     o_vernier_error,

    // "B" clock (i_fclk) synchronized inputs
    input logic                      i_fclk, // foreign clock
    input logic                      i_fclk_mark, 
    input logic [RAT_PREC_BITS-1:0]  i_fclk_mark_phase,//input for b time in TU 

    // "A" clock (clk) synchronized outputs
    output logic                     o_mark,
    output logic [RAT_PREC_BITS-1:0] o_mark_phase
   );

  localparam FIFO_DEPTH_BITS = $clog2(SAMPLE_TIMES)+1;
  assign o_vernier_ready = '1; // XXX TODO
  assign o_vernier_error = '0; // XXX TODO

  logic [RAT_PREC_BITS+1-1:0]         mark_position_d;

  logic [RAT_PREC_BITS-1:0]          rawphase;

  logic evt;
  logic evt1; // delayed -- mark_position delays calc by 1 cyc.
   
  logic [FCLK_DIV_BITS - 1:0] position;

  tsucnt #(.FCLK_DIV_BITS(FCLK_DIV_BITS),
           .FCLK_RST_BITS(FCLK_RST_BITS),
           .RAT_PREC_BITS(RAT_PREC_BITS)
          ) 
     u_tsucnt
          (.clk                         ,
           .rst_n                       ,
           .i_fclk                      ,
           .i_fclk_div                  ,
           .i_fclk_rst_cycs             , // needs to go away
           .i_num                       ,
           .i_denom                     ,
           .o_evt                       (evt),
           .position                    ,
           .o_phase                     (rawphase)
          );          
  mark_position # (.FIFO_DEPTH_BITS(FIFO_DEPTH_BITS),
                     .FIFO_WIDTH(RAT_PREC_BITS),
                     .SAMPLE_TIMES(SAMPLE_TIMES),
                     .RAT_PREC_BITS(RAT_PREC_BITS)
                    )
  u_mark_position
                   (.clk                    , 
                    .rst_n                  ,           
                    .i_fclk                 ,          
                    .i_fclk_div             ,
                    .i_fclk_mark          ,   
                    .i_evt                  (evt),
                    .o_evt                  (evt1),
                    .position               ,
                    .i_fclk_mark_phase              ,
                    .mark_position_d
                   );

  logic [RAT_PREC_BITS-1:0]    max0_q   , min0_d   , min0_q;

  always_ff @(posedge clk or negedge rst_n) 
    min0_q    <= rst_n ? min0_d    : '0;

  // invariant: max0 = min0 + i_denom*i_fclk_div - 1
  // meaning: min0 is smallest phase
  //          max0 is largest phase
  //
  // note that the math here is mod 2^N
  // max, min, and rawphase are all maintained mod 2^N
  // therefore we cannot use < and >, instead we have to use operators
  // "to the right of" and "to the left of" (less than one half time the
  // way around the number circle)
  //
  assign max0_q = min0_q + i_denom*i_fclk_div-1;

  logic  max_up, min_dn;
`define RIGHTOF(x,y) (~(((x-y)>>(RAT_PREC_BITS-1))&1'b1))
  
  assign max_up = `RIGHTOF(rawphase,max0_q); // rawphase to right of max
  assign min_dn = `RIGHTOF(min0_q,rawphase); // min to right of rawphase

  // update min0, max0:
  //
  // if max_up, we've overrun range upwards, update so that phase is the
  // new max
  //
  // if max_dn, we've overrun range downwards, update do that the phase is
  // the new min

  always_comb begin
    min0_d = min0_q;
    if (i_vernier_start)
      min0_d = 0;
    else if (max_up)
      min0_d = rawphase + 1 - i_denom*i_fclk_div;
      // here max0_d (which doesnt exist) should be equal to phase
    else if (min_dn)
      min0_d = rawphase;
  end

  logic                              mark_v;
  logic [RAT_PREC_BITS-1:0]          mark_phase_b;
  
  // top bit of mark_position is valid bit
  assign mark_v       = mark_position_d[$bits(mark_position_d)-1];
  assign mark_phase_b = mark_position_d[$bits(mark_position_d)-2:0];

  logic                              do_output;
  assign do_output = mark_v & evt1;

  logic [RAT_PREC_BITS-1:0]          edge_age, result;

  // this is a bit tricky.  In earlier implementations, we compared
  // phase to min0_d as baseline, to avoid getting a negative edge_age.
  // but here we will use min0_q as baseline.
  // During operation, changes in min0/max0 are normally
  // rare in any case, and usually by a small number of counts.
  assign edge_age = rawphase - min0_q;

  assign result =         
        edge_age                  // age of B edge
      + mark_phase_b              // input age in B clock dom.
      + i_num*(SAMPLE_TIMES)      // synchronizer delay
      + i_num;                    // output delay (1 A cycle to receiver)

  always_ff @(posedge clk) begin
    o_mark <= do_output;
      
    if(~rst_n | do_output)
      o_mark_phase <= rst_n ? result : '0;
  end
endmodule
 
module mark_position
   # (parameter FIFO_DEPTH_BITS =  2,
      parameter FIFO_WIDTH =  37,
      parameter RAT_PREC_BITS = 32,
      parameter FCLK_DIV_BITS = 3,
      parameter SAMPLE_TIMES = 2
   )
(   
    // "B" foreign-clock inputs
    input logic                             i_fclk, 
    input logic                             i_fclk_mark, // input mark
    input logic [RAT_PREC_BITS-1:0 ]        i_fclk_mark_phase,   // input phase
    
    // "A" 1588 clock inputs & outputs
    input logic                             clk,         // 1588 clock
    input logic                             rst_n,       // 1588 reset
    input logic [FCLK_DIV_BITS-1:0]         i_fclk_div,  // stable
    input logic                             i_evt,
    output logic                            o_evt,
    input logic [FCLK_DIV_BITS-1:0]         position,

    output logic [RAT_PREC_BITS+1-1:0]      mark_position_d
);

  localparam FIFO_DEPTH=(1 << FIFO_DEPTH_BITS);
  logic [FIFO_DEPTH-1:0][RAT_PREC_BITS+1-1:0] fifo;

  //////////////////////////////////////////////////////////////////////
  // "B" clock (i_fclk) input side of FIFO
  logic [FIFO_DEPTH_BITS-1:0] wr_pointer;
  // XXX BUG!
  // need to synchronize rst_n into "B" domain here
  always_ff @(posedge i_fclk or negedge rst_n)  begin
    if(!rst_n) // NO.. this won't work.. rst_n is not on B clock
      wr_pointer  <= 0;
    else if(position == i_fclk_div-1) // ?
      wr_pointer  <= wr_pointer + 1;
  end 
   
  logic firstcycle;
  assign firstcycle= position == 0;

  always_ff @(posedge i_fclk or negedge rst_n)    
    // Store position information + mark information into a FIFO
    begin
      if(!rst_n)
        fifo <= '0;
      else if(firstcycle)
        fifo[wr_pointer] <= {i_fclk_mark, i_fclk_mark_phase};
      else if(i_fclk_mark)
        fifo[wr_pointer] <= {i_fclk_mark, i_fclk_mark_phase};
    end

  //////////////////////////////////////////////////////////////////////
  // "A" clock (clk) output side of FIFO
  logic [FIFO_DEPTH_BITS-1:0] rd_pointer;
  
  always_ff @(posedge clk or negedge rst_n)   
    // Retrieve position information from FIFO every sflck positive edge
    begin
      if(!rst_n) begin
        rd_pointer      <= '0;
        mark_position_d <= '0;
        o_evt             <= '0;
      end else if(i_evt) begin
        rd_pointer      <= rd_pointer + 1;
        mark_position_d <= fifo[rd_pointer];
      end;

      o_evt <= i_evt;
    end

endmodule
