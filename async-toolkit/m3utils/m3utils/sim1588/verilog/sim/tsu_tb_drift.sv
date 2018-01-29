`include "tsu.sv"
`timescale 1ps/1fs

module tsu_tb_marker #() () ;

  localparam FCLK_DIV_BITS= 3;
  localparam FCLK_RST_BITS=16;
  localparam FCLK_MIN_BITS=16;
  localparam CNT_PREC_BITS=64;
  localparam RAT_PREC_BITS=32;
  localparam O_PREC_BITS=64;
  localparam FIFO_DEPTH= 3;
  localparam FIFO_WIDTH    =  7;
  localparam SAMPLE_TIMES  =  2;


  logic                        clk;             // 1588 clock
  logic                        rst_n;           // 1588 reset

  logic                        i_fclk;          // foreign clock
  logic [FCLK_DIV_BITS-1:0]    i_fclk_div;      // foreign clock div.
  logic [FCLK_RST_BITS-1:0]    i_fclk_rst_cycs; // reset cycles
  logic [FCLK_MIN_BITS-1:0]    i_fclk_min_cycs; // cycles for min.
    
  logic [RAT_PREC_BITS-1:0]    i_num;           // speed of fclk
  logic [RAT_PREC_BITS-1:0]    i_denom;         // speed of 1588 clk
    
  logic [RAT_PREC_BITS-1:0]    o_phase;
  logic                        o_phase_v;
  
  logic                        i_fclk_marker;
  logic [RAT_PREC_BITS-1:0]    i_phase_b;
  logic                        vernier_start;


  // f_1588 = fnum_1588 / fden_1588 THz
  localparam fnum_1588 =         1;
  localparam fden_1588 =      1020; // 1020 ps cycle

  localparam fnum_fclk     =    1;
  localparam fden_fclk     = 2048; // 2048 ps cycle

  localparam gcd           = 1; // use picoseconds! (actual GCD is 20)
  
  localparam frac_err  = 0.0001; //100ppm
  localparam frac_err_average_cycles = 10000; // expect a state change in about this many cycles ???
    
  tsu #(.FCLK_DIV_BITS (FCLK_DIV_BITS),
        .FCLK_RST_BITS (FCLK_RST_BITS),
        .FCLK_MIN_BITS (FCLK_MIN_BITS),
        .CNT_PREC_BITS (CNT_PREC_BITS),
        .RAT_PREC_BITS (RAT_PREC_BITS),
        .FIFO_WIDTH    (FIFO_WIDTH),
        .SAMPLE_TIMES  (SAMPLE_TIMES),
        .O_PREC_BITS   (O_PREC_BITS)
       )
       u_dut
       (
        .*
       );

  // generate 1588 clock

  real last_delta, last_fclk, last_delta_dlyd;
  real err, q;
  real last_marker, last_a;

  localparam LAST_DELTA_DELAY=3;  // how many to remember
  real last_delta_q[LAST_DELTA_DELAY:0];

  assign last_delta_q[0] = last_delta;
  assign last_delta_dlyd = last_delta_q[LAST_DELTA_DELAY];

  generate
    for (genvar i=1; i<=LAST_DELTA_DELAY; ++i) begin : gen_last_delta_q_blk
      always_ff @(posedge clk) last_delta_q[i] <= last_delta_q[i-1];
    end
  endgenerate

  initial begin
    vernier_start = 0;
    #(201*fden_1588/fnum_1588/2.0);
    #(fden_1588*fden_fclk/fnum_1588/fnum_fclk);
    vernier_start = 1;
    #(fden_1588/fnum_1588);
    vernier_start = 0;
  end

  initial begin : gen_1588clk
    clk = '0;
    while (1) begin
      #(fden_1588*1.0/fnum_1588/2.0);
      clk = ~clk;
//      if (clk) last_delta = $realtime - last_fclk;
//        if(clk) last_a = $realtime;
//      if (o_phase_v) last_delta = last_a - last_marker; 
    end
  end

  // generate target clock

  logic fclk_go;
  
  initial begin
    fclk_go = 1;
    
    i_fclk = '0;
    #(200*fden_1588/fnum_1588/2.0); // wait 100 1588 cycles
    while (fclk_go) begin
      q = $urandom_range(frac_err_average_cycles*2,0);
      if      (q==0) err = -1;
      else if (q==1) err =  0;
      else if (q==2) err = +1;

      #(fden_fclk*1.0/fnum_fclk/2.0*(1.0+err*frac_err));
//      #(fden_fclk*1.0/fnum_fclk/2.0);
      i_fclk = ~i_fclk;

      if (i_fclk) last_fclk = $realtime;
    end
  end

/*  initial begin
    i_fclk = '0;
    #(200*fden_1588/fnum_1588/2.0); // wait 100 1588 cycles
    while (1) begin
      #(fden_fclk*1.0/fnum_fclk/2.0);
      i_fclk = ~i_fclk;
      if (i_fclk) last_fclk = $realtime;
    end
  end */


  // reset
  
  initial begin
    rst_n = '0;
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    rst_n = '1;

  end

  initial begin
    i_fclk_marker = '0;
    repeat (200) @(posedge i_fclk);
    
    repeat (5000) begin
      @(negedge i_fclk);
      i_fclk_marker = 1;

      @(posedge i_fclk);
      last_marker = $realtime;

      @(negedge i_fclk);
      i_fclk_marker = 0;

      repeat($urandom_range(100,10))
        @(posedge i_fclk);
    end
    fclk_go = 0;

    repeat(10) @(posedge clk);
    
    $stop();
  end
  
  initial 
    while(1) begin
      real mynow;

      @(posedge clk);
      mynow = $realtime;
      @(negedge clk);
      if (o_phase_v) begin
        last_a = mynow;
        last_delta = mynow - last_marker;
      end
  end

  
//  always_ff @(posedge i_fclk)
//    std::randomize(i_fclk_marker);
  
  // config. inputs
  assign i_fclk_div      =                        1;
  assign i_fclk_rst_cycs =                      100;
  assign i_fclk_min_cycs =                      257;
  assign i_num           =  fden_1588*fnum_fclk/gcd;
  assign i_denom         =  fnum_1588*fden_fclk/gcd;
  assign i_phase_b       =                       '0;
  
  real measurement,measurement_temp ;
  real t_error, t_error_temp;
  
  assign measurement = o_phase*1.0 / i_num * fden_1588*1.0/fnum_1588; // ps

  assign t_error = measurement-last_delta;

  // how to take out the t_error at the right moment?

  real t_error_reg;
  
  initial 
    while(1) begin
      @(negedge clk);
      #1;
      
      if (o_phase_v)
        t_error_reg = t_error;
    
  end
  


endmodule
