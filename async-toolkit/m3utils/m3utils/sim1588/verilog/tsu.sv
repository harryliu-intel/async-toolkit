
//
// Vernier Timestamping 
//
// Author : Mika Nystrom <mika.nystroem@intel.com>
// May, 2015
//

`include "tsu_pkg.vh"

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
    parameter CNT_PREC_BITS=64,
    parameter RAT_PREC_BITS=32
   )
   (
    input  logic                            clk,             // 1588 clock
    input  logic                            rst_n,           // 1588 reset

    input  logic                            i_fclk,          // foreign clock
    input  logic [FCLK_DIV_BITS-1:0]        i_fclk_div,      // fgn clock div.
    input  logic [FCLK_RST_BITS-1:0]        i_fclk_rst_cycs, // reset cycles
    
    input  logic [RAT_PREC_BITS-1:0]        i_num,           // 1588 period
    input  logic [RAT_PREC_BITS-1:0]        i_denom,         // fclk period
    
    input  logic                            i_dec_phase,     // -2*i_num
    input  logic                            i_inc_phase,     // +2*i_num
    input  logic                            i_fclk_marker,   // input marker
    input  logic signed [RAT_PREC_BITS-1:0] i_phase_b,       // input phase
    
    output logic                            o_evt,           // rising edge det
    output logic signed [RAT_PREC_BITS-1:0] phase_b,
    output logic [FCLK_DIV_BITS-1:0]        position,  
     
    output logic signed [RAT_PREC_BITS-1:0] o_phase
   );


  localparam NUM_DIV_FLOPS=(1<<FCLK_DIV_BITS);
  logic [NUM_DIV_FLOPS:0] marker_div;
  logic                   f_rst_n;
  logic signed [FCLK_DIV_BITS :0]      i_fclk_div_smaller;
  assign i_fclk_div_smaller = i_fclk_div -1;

  always_ff @(posedge i_fclk or negedge rst_n)
    begin
      if(!rst_n)
      begin
       position <= '1;
      end
      else if(position < (i_fclk_div_smaller))
        begin
        position <= position + 1;
        end
      else begin
        position <= '0;
        end 
    end

  logic div_clk;
  assign div_clk = (position < (i_fclk_div/2)) ? 1:0;

  logic sfclk;    
  assign sfclk = (i_fclk_div != 1) ? div_clk:i_fclk;

  assign phase_b = -i_denom*position + i_phase_b;

  sync2xdff0_x2 u_sync2xdff0_x2_sampler (
    .q   ( samp   ),
    .ck  ( clk    ), // 1588 clock
    .d   ( sfclk  ), // divided foreign clock
    .se  ( 1'b0   ),
    .si  ( 1'b0   )
  );

  logic psamp;

  always_ff @(posedge clk)
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

  always_ff @(posedge clk) begin
    assert (rst_n !=='1 | ~i_inc_phase | ~i_dec_phase)
      else $error("up/down not exclusive");
  end

  always_comb begin
    p_incr = +i_num;

    if      (i_dec_phase)
      p_incr = -i_num;
    else if (i_inc_phase)
      p_incr = +i_num+(i_num<<1); // 3*i_num
  end

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
    parameter FCLK_RST_BITS = 16,
    parameter FCLK_MIN_BITS = 16,
    parameter CNT_PREC_BITS = 64,
    parameter RAT_PREC_BITS = 32,
    parameter FIFO_WIDTH    =  7,
    parameter SAMPLE_TIMES  =  2,
    parameter O_PREC_BITS   = 64,
    parameter ZERO_PHASE    =  0   // auto-zero phase counter
   )
   (
    input  logic                        clk,             // 1588 clock
    input  logic                        rst_n,           // 1588 reset

    input  logic                        i_fclk,          // foreign clock
    input  logic [FCLK_DIV_BITS-1:0]    i_fclk_div,      // foreign clock div.
    input  logic [FCLK_RST_BITS-1:0]    i_fclk_rst_cycs, // reset cycles
    input  logic [FCLK_MIN_BITS-1:0]    i_fclk_min_cycs, // cycles for min.
    input  logic                        i_fclk_marker, 
    
    input  logic [RAT_PREC_BITS-1:0]    i_num,           // 1588 period
    input  logic [RAT_PREC_BITS-1:0]    i_denom,         // fclk period

    input  logic [RAT_PREC_BITS-1:0]    i_phase_b,       //input for b time in TU         
    input  logic                        vernier_start,
    
    output logic                        o_phase_v,
    output logic signed [RAT_PREC_BITS-1:0]    o_phase
    
   );

  localparam FIFO_DEPTH_BITS = $clog2(SAMPLE_TIMES)+1;
  
  import tsu_pkg::*; 
  
  tsu_pkg::info i_marker_position;

  logic signed [RAT_PREC_BITS-1:0]    phase;
  logic signed [RAT_PREC_BITS-1:0]    phase_b;

  logic evt;
  logic evt1; // delayed -- marker_position delays calc by 1 cyc.
   
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
           .i_fclk_rst_cycs             , // 
           .i_num                       ,
           .i_denom                     ,
           .i_dec_phase                 (dec_phase),
           .i_inc_phase                 (inc_phase), // 
           .i_phase_b                   ,
           .i_fclk_marker               ,
           .o_evt                       (evt),
           .phase_b                     (phase_b),
           .position                    (position),
           .o_phase                     (phase)
          );          
  marker_position # (.FIFO_DEPTH_BITS(FIFO_DEPTH_BITS),
                     .FIFO_WIDTH(FIFO_WIDTH),
                     .SAMPLE_TIMES(SAMPLE_TIMES)
                    )
  u_marker_position
                   (.clk                    , 
                    .rst_n                  ,           
                    .i_fclk                 ,          
                    .i_fclk_div             ,
                    .i_fclk_marker          ,   
                    .i_evt              (evt),
                    .o_evt             (evt1),
                    .position          (position),
                    .phase_b           (phase_b),
                    .o_marker_position (i_marker_position)     
                   );

  assign dec_phase = ZERO_PHASE ? (phase > 1000) : '0;
  assign inc_phase = ZERO_PHASE ? (phase < -1000) : '0;

  logic signed [RAT_PREC_BITS-1:0]    p_corr_d, p_corr_q;

  // this is the in-range corrector
  assign p_corr_d = (dec_phase ? -(i_num<<1) : (inc_phase ? (i_num<<1) : '0));

  // inc_phase and dec_phase as they apply to phase are delayed a cycle
  // therefore we need to delay them a cycle for min0_d as well

  always_ff @(posedge clk)
    p_corr_q <= rst_n ? p_corr_d : '0;
  
  logic signed [RAT_PREC_BITS-1:0]    max0_q   , min0_d   , min0_q;

  always_ff @(posedge clk) begin
    min0_q    <= rst_n ? min0_d    : '0;
  end

  // invariant: max0 = min0 + i_denom*i_fclk_div - 1
  // meaning: min0 is smallest phase
  //          max0 is largest phase 
  assign max0_q = min0_q + i_denom*i_fclk_div-1;

  logic  max_up, min_dn;
  assign max_up = phase >  max0_q;
  assign min_dn = phase <  min0_q;

  // update min0, max0:
  //
  // if max_up, we've overrun range upwards, update so that phase is the
  // new max
  //
  // if max_dn, we've overrun range downwards, update do that the phase is
  // the new min

  always_comb begin
    min0_d = min0_q;
    if (vernier_start)
      min0_d = 0;
    else if (max_up)
      min0_d = phase + 1 - i_denom*i_fclk_div;
      // here max0_d (which doesnt exist) should be equal to phase
    else if (min_dn)
      min0_d = phase;

    min0_d += p_corr_q;
  end

  logic                     do_output;

  assign do_output = i_marker_position.marker_v & evt1;

  logic signed [RAT_PREC_BITS-1:0] edge_age;
  assign edge_age = phase - min0_d;
  
  always_ff @(posedge clk) begin
    o_phase_v <= do_output;
    if(do_output)
      o_phase <= 
        edge_age                  // age of B edge
      + i_marker_position.phase_b // input age in B clock dom.
      + i_num*(SAMPLE_TIMES)      // synchronizer delay
      + i_num;                    // output delay (1 A cycle to receiver)
  end

  logic [RAT_PREC_BITS-1:0] debug_o_phase;
  logic [RAT_PREC_BITS-1:0] debug_o_phase_2;

  assign debug_o_phase = i_denom*position; 
  assign debug_o_phase_2 = i_denom*(i_fclk_div-position);

endmodule
 
module marker_position
   # (parameter FIFO_DEPTH_BITS =  2,
      parameter FIFO_WIDTH =  37,
      parameter RAT_PREC_BITS = 32,
      parameter FCLK_DIV_BITS = 3,
      parameter SAMPLE_TIMES = 2
   )
(   input  logic                        clk,             // 1588 clock
    input  logic                        rst_n,           // 1588 reset

    input  logic                        i_fclk,          // foreign clock
    input  logic [FCLK_DIV_BITS-1:0]    i_fclk_div,

    input  logic                        i_fclk_marker,   // marker of foreign clock
    input  logic signed [RAT_PREC_BITS-1:0 ]   phase_b,
    input  logic                        i_evt,
    output logic                        o_evt,
    input  logic [FCLK_DIV_BITS-1:0]    position,

    output tsu_pkg::info                o_marker_position
);

  localparam FIFO_DEPTH=(1 << FIFO_DEPTH_BITS);
  
  import tsu_pkg::*; 
  
  tsu_pkg::info marker_info;   //packed information about marker

  tsu_pkg::info [FIFO_DEPTH-1:0] fifo ;
  tsu_pkg::info debug_0;
  tsu_pkg::info debug_1;
  tsu_pkg::info debug_2;
  tsu_pkg::info debug_3;

  logic [$clog2(FIFO_DEPTH)-1:0] pointer ;


  always_ff @(posedge i_fclk or negedge rst_n)
    begin
      if(!rst_n)
      begin
       pointer  <= 0;
      end
      else if(position >= i_fclk_div-1)
      begin
        pointer  <= pointer + 1;
      end 
    end  //end always_ff
   
  logic firstcycle;

  assign firstcycle= position == 0;

  always_ff @(posedge i_fclk or negedge rst_n)    // Store position information + marker information into a FIFO
    begin
      if(!rst_n)
        fifo <= '{default:34'b0};
      else if(firstcycle)
        fifo[pointer] <= {i_fclk_marker, phase_b};
      else if(i_fclk_marker)
        fifo[pointer] <= {i_fclk_marker, phase_b};
    end
  assign debug_0 = fifo[0];
  assign debug_1 = fifo[1];
  assign debug_2 = fifo[2];
  assign debug_3 = fifo[3];

  logic [$clog2(FIFO_DEPTH)-1 :0] read_pointer;
  
  always_ff @(posedge clk or negedge rst_n)   
  // Retrieve position information from FIFO every sflck positive edge
    begin
      if(!rst_n)
      begin
        read_pointer      <= '0;
        o_marker_position <= '0;
        o_evt             <= '0;
      end
      else if(i_evt)
      begin
        read_pointer      <= read_pointer + 1;
        o_marker_position <= fifo[read_pointer];
      end;

      o_evt <= i_evt;
    end

endmodule
