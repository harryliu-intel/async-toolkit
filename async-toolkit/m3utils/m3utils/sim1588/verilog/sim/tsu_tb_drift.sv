`include "tsu.sv"
`timescale 1ps/1fs

module tsu_tb_mark #() () ;

  localparam FCLK_DIV_BITS= 3;
  localparam RAT_PREC_BITS=24;
  localparam SAMPLE_TIMES  =  2;
  parameter FCLK_RST_BITS = 16;

  logic                        clk;             // 1588 clock
  logic                        rst_n;           // 1588 reset

  logic                        i_fclk;          // foreign clock
  logic [FCLK_DIV_BITS-1:0]    i_fclk_div;      // foreign clock div.
    
  logic [RAT_PREC_BITS-1:0]    i_num;           // 1588 cycle time
  logic [RAT_PREC_BITS-1:0]    i_denom;         // foreign cycle time
  logic [RAT_PREC_BITS-1:0]    i_denom_err; 

  logic [FCLK_RST_BITS-1:0]    i_fclk_rst_cycs; // reset cycles XXX remove

  logic                        o_mark;
  logic [RAT_PREC_BITS-1:0]    o_mark_phase;
  
  logic                        i_fclk_mark;
  logic [RAT_PREC_BITS-1:0]    i_fclk_mark_phase;
  logic                        i_vernier_start;
  logic                        o_vernier_ready;
  logic                        o_vernier_error;

  // f_1588 = fnum_1588 / fden_1588 THz
  localparam fnum_1588 =         1;
  localparam fden_1588 =      1020; // 1020 ps cycle

  localparam fnum_fclk     =    1;
  localparam fden_fclk     = 2048; // 2048 ps cycle

  localparam gcd           = 1; // use picoseconds! (actual GCD is 20)
  
//  localparam frac_err  = 0.0001; //100ppm
//  localparam frac_drift= 0.01; //10000ppm

  localparam frac_err = 0;
  localparam frac_drift = 0;

  
  localparam frac_err_average_cycles = 10000; // expect a state change in about this many cycles ???
    
  tsu #(.FCLK_DIV_BITS (FCLK_DIV_BITS),
        .FCLK_RST_BITS (FCLK_RST_BITS),
        .RAT_PREC_BITS (RAT_PREC_BITS),
        .SAMPLE_TIMES  (SAMPLE_TIMES)
       )
       u_dut
       (
        .*
       );

  // generate 1588 clock

  real last_delta, last_fclk, last_delta_dlyd;
  real err, q;
  real last_mark, last_a;

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
    i_vernier_start = 0;
    #(201*fden_1588/fnum_1588/2.0);
    #(fden_1588*fden_fclk/fnum_1588/fnum_fclk);
    i_vernier_start = 1;
    #(fden_1588/fnum_1588);
    i_vernier_start = 0;
  end

  initial begin : gen_1588clk
    clk = '0;
    while (1) begin
      #(fden_1588*1.0/fnum_1588/2.0);
      clk = ~clk;
//      if (clk) last_delta = $realtime - last_fclk;
//        if(clk) last_a = $realtime;
//      if (o_phase_v) last_delta = last_a - last_mark; 
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

      #(fden_fclk*1.0/fnum_fclk/2.0*(1.0+err*frac_err+frac_drift));
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
    i_fclk_mark = '0;
    repeat (200) @(posedge i_fclk);
    
    repeat (50000) begin
      @(negedge i_fclk);
      i_fclk_mark = 1;

      @(posedge i_fclk);
      last_mark = $realtime;

      @(negedge i_fclk);
      i_fclk_mark = 0;

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
      if (o_mark) begin
        last_a = mynow;
        last_delta = mynow - last_mark;
      end
  end

  
//  always_ff @(posedge i_fclk)
//    std::randomize(i_fclk_mark);
  
  // config. inputs
  initial begin
    i_fclk_div        =                        4;
    i_fclk_rst_cycs   =                      100;
    i_num             =  fden_1588*fnum_fclk/gcd;
    i_denom           =  fnum_1588*fden_fclk/gcd;
    i_denom_err       =                       '0;
    i_fclk_mark_phase =                       '0;
  end
  
  real measurement,measurement_temp ;
  real t_error, t_error_temp;
  
  assign measurement = o_mark_phase*1.0 / i_num * fden_1588*1.0/fnum_1588; // ps

  assign t_error = measurement-last_delta;

  // how to take out the t_error at the right moment?

  real t_error_reg;
  
  initial 
    while(1) begin
      @(negedge clk);
      #1;
      
      if (o_mark)
        t_error_reg = t_error;
    
  end
  


endmodule
