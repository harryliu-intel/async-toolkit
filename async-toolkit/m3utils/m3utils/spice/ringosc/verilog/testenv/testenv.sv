


// /nfs/site/disks/zsc9_fwr_ip/ENGCKGEN/rp1/engckgen.v
//
// module engckgen ( cka, ckb, ckc, ckd, ckdiv4, vcc, vssx, engclk, rstb, sync, te );
//
// output  cka, ckb, ckc, ckd, ckdiv4;
//
// inout  vcc, vssx;
//
// input  engclk, rstb, te; // te is output enable
//
// input [1:0]  sync; // sets phasing of clocks


//////////////////////////////////////////////////////////////////////
//
// /nfs/site/disks/zsc9_fwr_analog_001/ganesani/ganesani_1278p3_p9pdk/FWR_0p9PDK/netlists/verilog/clk_rec_top.v
//
// module clk_rec_top ( cp, vcc, vssx, clk, i_rstval, refc_o_dly_s0, refc_o_dly_s1, rst_n, sel_odd_sync );
// cp is primary output
// output  cp;
//
// inout  vcc, vssx;
//
// input  clk, refc_o_dly_s0, refc_o_dly_s1, rst_n, sel_odd_sync;
//
// input [7:0]  i_rstval;

module clk_rec_top
  #()
   (output logic          cp,
    wire           vcc,
    wire           vssx,
    input logic           clk,
    input logic [ 7 : 0 ] i_rstval,
    input logic           refc_o_dly_s0,
    input logic           refc_o_dly_s1,
    input logic           rst_n,
    input logic           sel_odd_sync);

   // this doesn't have the actual real behavior, but good enough
   // for simple DV.
   assign cp = clk;
   
endmodule

//////////////////////////////////////////////////////////////////////
//
// module add32_wrapper 
//  #()
//   (input  logic [ 31:0 ] a,
//    input  logic [ 31:0 ]  b, 
//
//    output logic [ 31:0 ] s,
//
//    wire          vcc,
//    wire          vssx);


module Sigma1
  // /p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/dsibase_ulvt/verilog/lib783_i0s_160h_50pp_dsibase_ulvt.v
  #()
   (input  logic [ 31 : 0 ] i_x,
    output logic [ 31 : 0 ] o,
    wire            vcc,
    wire            vssx);

   logic [ 31 : 0 ]         y0, y1, y2;

   assign y0 [  0 +: 30 ] = i_x [  2 +: 30 ];
   assign y0 [ 30 +:  2 ] = i_x [  0 +:  2 ];
 
   assign y1 [  0 +: 19 ] = i_x [ 13 +: 19 ];
   assign y1 [ 19 +: 13 ] = i_x [  0 +: 13 ];

   assign y2 [  0 +: 10 ] = i_x [ 22 +: 10 ];
   assign y2 [ 10 +: 22 ] = i_x [  0 +: 22 ];

   generate
      for (genvar i = 0; i < 32; ++i) 
        begin : gen_xor
           i0sxor003aa1n02x5 u_xor(.a(y0[i]), 
                                        .b(y1[i]), 
                                        .c(y2[i]), 
                                        .out0(o[i]), 
                                        .vcc, 
                                        .vssx);
        end
   endgenerate
endmodule // Sigma1

module Sigma0
  // /p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/dsibase_ulvt/verilog/lib783_i0s_160h_50pp_dsibase_ulvt.v
  #()
   (input  logic [ 31 : 0 ] i_x,
    output logic [ 31 : 0 ] o,
    wire            vcc,
    wire            vssx);

   logic [ 31 : 0 ]         y0, y1, y2;

   assign y0 [  0 +: 26 ] = i_x [  6 +: 26 ];
   assign y0 [ 26 +:  6 ] = i_x [  0 +:  6 ];
 
   assign y1 [  0 +: 21 ] = i_x [ 11 +: 21 ];
   assign y1 [ 21 +: 11 ] = i_x [  0 +: 11 ];

   assign y2 [  0 +:  7 ] = i_x [ 25 +:  7 ];
   assign y2 [  7 +: 25 ] = i_x [  0 +: 25 ];

   generate
      for (genvar i = 0; i < 32; ++i) 
        begin : gen_xor
           i0sxor003aa1n02x5 u_xor(.a(y0[i]), 
                                        .b(y1[i]), 
                                        .c(y2[i]), 
                                        .out0(o[i]), 
                                        .vcc, 
                                        .vssx);
        end
   endgenerate
endmodule // Sigma0

module logic_stage
  #()
   (input  logic [ 63 : 0 ]  i_x,
    output logic [ 63 : 0 ]  o_y,
    wire             vcc,
    wire             vssx);

   // this code doesn't claim to do anything interesting
   // but it does set up some fun timing paths

   logic [ 63 : 0 ]          x1;

   logic [ 31 : 0 ]          s0, s1, u;

   // shift by 16 bits
   assign x1 [  0 +: 48 ] = i_x [ 16 +: 48 ];
   assign x1 [ 48 +: 16 ] = i_x [  0 +: 16 ];

   Sigma0 u_sigma0(.i_x(i_x [ 31 : 0 ]), 
                   .o(s0) , 
                   .vcc, 
                   .vssx);
   Sigma1 u_sigma1(.i_x(x1  [ 31 : 0 ]), 
                   .o(s1) , 
                   .vcc, 
                   .vssx);

   add32_wrapper u_add32_wrapper( .a(s0), 
                                  .b(s1), 
                                  .s(u), 
                                  .vcc, 
                                  .vssx );
 
   assign o_y [ 15 :   0 ] = i_x [ 15 : 0 ];
   assign o_y [ 47 :  16 ] = u;
   assign o_y [ 63 :  48 ] = i_x [ 63 : 48 ];
   
endmodule // logic_stage

module latch32 
  #()
   (input  logic clk,
    input  logic [ 31 : 0 ]  i_d,
    output logic [ 31 : 0 ]  o_q,
    wire              vcc,
    wire              vssx);
   
   logic                     _clk;

   i0scinv00aa1n24x4 u_inv(.clk, .clkout(_clk), .vcc, .vssx);
   
   generate
      for (genvar i = 0; i < 32; ++i) 
        begin : gen_bits
           i0sltny00aa1n02x1 u_tny(.CLK(clk),
                                      .D(i_d[i]),
                                      .Q(o_q[i]),
                                      ._CLK(_clk),
                                      .vcc,
                                      .vssx);
        end
   endgenerate
endmodule // latch32

module latched_stage
  #()
   (input  logic clk,
    input  logic [ 63 : 0 ]  i_x,
    output logic [ 63 : 0 ]  o_y, // latched output
    wire             vcc,
    wire             vssx);

   logic [ 63 : 0 ]         yy;

   logic_stage u_stage(.i_x, .o_y(yy), .vcc, .vssx);

   latch32 u_latch0(.clk, 
                    .i_d(yy[  0 +: 32 ]), 
                    .o_q(o_y [  0 +: 32 ]),
                    .vcc, .vssx);
   latch32 u_latch1(.clk, 
                    .i_d(yy[ 32 +: 32 ]), 
                    .o_q(o_y [ 32 +: 32 ]), 
                    .vcc, .vssx);

endmodule // latched_stage

module testenv_datapath
  #(parameter STAGES=16) // m.b. divisible by 4
   (input  logic [ 3 : 0 ]   ck, // assumed to go 0 -> 1 -> 2 -> 3 -> 0 -> ...
    input  logic [ 63 : 0 ]  i_x, // transparent on ck[3]
    output logic [ 63 : 0 ]  o_y, // latched output, transparent on ck[0]
    wire             vcc,
    wire             vssx);

   // N.B. there is no reset here!

   logic [ 63 : 0 ]          z [ STAGES : 0 ];

   assign z[0] = i_x;
   
   generate
      for (genvar i = 0; i < STAGES; ++i)
        begin : gen_stages
           latched_stage 
                      u_latched_stage(.clk(ck [3 - (i % 4)]),
                                         .i_x(z[i]),
                                         .o_y(z[i + 1]),
                                         .vcc,
                                         .vssx);
        end
   endgenerate

   assign o_y = z[ STAGES ];
endmodule // testenv_datapath

module vshift_up
  #(parameter BITS=1)
   (input  logic [ BITS - 1 : 0 ]   i_lo,
    output logic [ BITS - 1 : 0 ]   o_hi,

    wire                    vcchi, 
    wire                    vcc, 
    wire                    vssx);

   // oversimplified digital model
   assign o_hi = i_lo;
   
endmodule // vshift_up

module vshift_dn
  #(parameter BITS=1)
   (input  logic [ BITS - 1 : 0 ]   i_hi,
    output logic [ BITS - 1 : 0 ]   o_lo,

    wire                    vcchi, 
    wire                    vcc, 
    wire                    vssx);

   // oversimplified digital model
   assign o_lo = i_hi;
   
endmodule // vshift_up

   
module ck_rotate
  #()
   (input logic [ 3 : 0 ] i_pck,
    output logic [ 3 : 0 ] o_ck,
    input logic [ 1 : 0 ]  i_ck_rot,
    wire            vcc,
    wire            vssx);

   generate
      for (genvar i = 0; i < 4; ++i) 
        begin : gen_mux
           i0smbn024aa1d12x5 u_mux
                      (.a(i_pck[(i + 0) % 4]),
                       .b(i_pck[(i + 1) % 4]),
                       .c(i_pck[(i + 2) % 4]),
                       .d(i_pck[(i + 3) % 4]),
                       .o(o_ck[i]),
                       .sa(i_ck_rot[0]),
                       .sb(i_ck_rot[1]),
                       .vcc,
                       .vssx);

        end // block: gen_mux
   endgenerate
endmodule // ck_rotate

module vflop
  #(parameter BITS=64)
   (input logic clk,
    input logic [ BITS - 1 : 0 ]  i_d,
    output logic [ BITS - 1 : 0 ] o_q,
    wire                   vcc,
    wire                   vssx);

   generate
      for (genvar i = 0; i < BITS; ++i)
        begin : gen_flop
           
           i0sfun000aa1d02x5 u_flop(.clk,
                                       .d(i_d[i]),
                                       .o(o_q[i]),
                                       .vcc,
                                       .vssx);
        end
   endgenerate
endmodule

module testenv
  #()
   (input logic             i_clk2x_hi,         // nominal "2X" (~1 GHz) clock
    input logic             i_rst_n_hi, // active-lo async reset
    
    input logic [ 63 : 0 ]  i_x_hi,
    output logic [ 63 : 0 ] o_y_hi, // registered output

    // config bits for clock recovery
    input logic [ 7 : 0]    i_rstval_hi, // reset value of recovery FSM
    input logic             i_refc_o_dly_s0_hi, // delay setting of recovery
    input logic             i_refc_o_dly_s1_hi, // delay setting of recovery
    input logic             i_sel_odd_sync_hi, // odd sync of recovery FSM

    // config bits for engckgen
    input logic [ 1 : 0 ]   i_sync_hi, // multiphase clock phasing
    input logic             i_te_hi, // clock enable  (s.b. 1)

    // clock-phase rotation
    input logic [ 1 : 0 ]   i_ck_rot_hi,

    // debug output
    output logic [ 3 : 0 ]  o_ck_hi,
    
    wire             vcchi, // nominal (~650 mV)
    wire             vcc, // low-voltage (~300mV)
    wire             vssx);


   // level-shift inputs
   logic                    clk2x;
   vshift_dn #(1)  u_ls_clk2x(.i_hi('{i_clk2x_hi}), 
                              .o_lo('{clk2x}), 
                              .vcchi, .vcc, .vssx);
   
   logic                    rst_n;
   vshift_dn #(1)  u_ls_rst_n(.i_hi('{i_rst_n_hi}), 
                              .o_lo('{rst_n}),
                               .vcchi, .vcc, .vssx);

   logic [ 7 : 0 ]          rstval;
   vshift_dn #(8)  u_ls_rstval(.i_hi(i_rstval_hi),
                               .o_lo(rstval),
                               .vcchi, .vcc, .vssx);                               
   
   logic                    refc_o_dly_s0,refc_o_dly_s1, sel_odd_sync;
   vshift_dn #(3)  u_ls_refc_o_dly(.i_hi('{i_refc_o_dly_s0_hi,
                                           i_refc_o_dly_s1_hi,
                                           i_sel_odd_sync_hi}),
                                   .o_lo('{refc_o_dly_s0,
                                           refc_o_dly_s1,
                                           sel_odd_sync}),
                                   .vcchi, .vcc, .vssx);
                     
   logic [ 1 : 0 ]          sync;
   vshift_dn #(2)  u_ls_sync(.i_hi(i_sync_hi),
                               .o_lo(sync),
                               .vcchi, .vcc, .vssx);
   
   logic                    te;
   vshift_dn #(1)  u_ls_te(.i_hi('{i_te_hi}), 
                           .o_lo('{te}),
                           .vcchi, .vcc, .vssx);
   
   logic [ 1 : 0 ]          ck_rot;
   vshift_dn #(2)  u_ls_ck_rot(.i_hi(i_ck_rot_hi),
                               .o_lo(ck_rot),
                               .vcchi, .vcc, .vssx);
   // the clock generation logic
   logic                    clk4x; // the recovered 4X clock
    
   // 1. the clock recovery circuit
   clk_rec_top u_clk_rec_top( .cp(clk4x),
                              .vcc,
                              .vssx,
                              .clk(clk2x),
                              .i_rstval(rstval),
                              .refc_o_dly_s0,
                              .refc_o_dly_s1,
                              .rst_n,
                              .sel_odd_sync );
   
   // the phases of the multiphase clock
   logic [ 3:0 ]            pck, ck, ck_hi;

   
   // 2. the engckgen
   engckgen u_engckgen(.cka(pck[0]),
                       .ckb(pck[1]),
                       .ckc(pck[2]),
                       .ckd(pck[3]),
                       .ckdiv4(), // N.C.
                       .vcc,
                       .vssx,
                       .engclk(clk4x),
                       .rstb(rst_n),
                       .sync,
                       .te);

   // rotate the pcks to ck
   ck_rotate u_ck_rotate(.i_pck(pck), 
                         .o_ck(ck),
                         .i_ck_rot(ck_rot),
                         .vcc, 
                         .vssx);
   
   vshift_up #(4) u_ls_ck(.i_lo(ck), .o_hi(ck_hi), .vcchi, .vcc, .vssx);

   assign o_ck_hi = ck_hi; // for debug

   // The whole datapath follows

   // the input flop, high-voltage domain

   logic [ 63 : 0 ]         x_hi, x;
   
   vflop u_vflop_x(.clk(ck_hi[0]), // -> ck[3]
                   .i_d(i_x_hi),
                   .o_q(x_hi),
                   .vcc(vcchi),
                   .vssx);

   // level shifters down
   vshift_dn #(64) u_ls_x(.i_hi(x_hi), .o_lo(x), .vcchi, .vcc, .vssx);

   // the main datapath, low-voltage domain
   logic [ 63 : 0 ]         y, y_hi, yq_hi;
   
   testenv_datapath u_testenv_datapath(.ck, 
                                       .i_x(x), 
                                       .o_y(y), 
                                       .vcc, 
                                       .vssx);
   
   
   // level shifters up
   vshift_up #(64) u_ls_y(.i_lo(y), .o_hi(y_hi), .vcchi, .vcc, .vssx);
   
   // the output flops, high-voltage domain
   vflop u_vflop_y0(.clk(ck_hi[3]), // <- ck[0]
                    .i_d(y_hi),
                    .o_q(yq_hi),
                    .vcc(vcchi),
                    .vssx);

   vflop u_vflop_y1(.clk(ck_hi[0]), // <- ck[3] (get it on the right phase)
                    .i_d(yq_hi),
                    .o_q(o_y_hi),
                    .vcc(vcchi),
                    .vssx);
   
   
endmodule // testenv


    
