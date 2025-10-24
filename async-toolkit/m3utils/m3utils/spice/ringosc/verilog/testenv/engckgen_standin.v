// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// Verilog HDL and netlist files of
// "cpark1_lib engckgen schematic"
// Library - cpark1_lib, Cell - engckgen, View - schematic
// LAST TIME SAVED: Nov 20 16:32:12 2023
// NETLIST TIME: Nov 20 16:33:05 2023

module engckgen ( cka, ckb, ckc, ckd, ckdiv4, vcc, vssx, engclk, rstb, sync, te );

output  cka, ckb, ckc, ckd, ckdiv4;

inout  vcc, vssx;

input  engclk, rstb, te;

input [1:0]  sync;

wire cin , c1b , c1 , net17 , net13 , net15 , c2 , net23 , net56 , net6 , net32 , c3 , net61 , net39 , c4 , net47 , net48 , net53 , net2 , net62 , net70 , net65 , net34 , net73 , net74 , net1 , net76 , net77 , net78 , net4 , net80 , net5 , net112 , net119 , net126 , net132 , rst , net136 , t1 , net3 , net142 , clkb , net9 , net155 , net7 , net163 , clk ;
 


i0szvcc00aa1n00x5  \cc[3]  ( .vcc(vcc), .vssx(vssx));
i0szvcc00aa1n00x5  \cc[2]  ( .vcc(vcc), .vssx(vssx));
i0szvcc00aa1n00x5  \cc[1]  ( .vcc(vcc), .vssx(vssx));
i0szvcc00aa1n00x5  \cc[0]  ( .vcc(vcc), .vssx(vssx));
i0scinv00aa1n15x5  ckinv1 ( .clkout(clkb), .vcc(vcc), .vssx(vssx), .clk(engclk));
i0sfan018aa1d12x5  fan06 ( .ob(net73), .vcc(vcc), .vssx(vssx), .clk(clk), .d(net78), .s(rst));
i0sfan018aa1d12x5  fan05 ( .ob(net80), .vcc(vcc), .vssx(vssx), .clk(clk), .d(net77), .s(rst));
i0sfan018aa1d12x5  fan04 ( .ob(net61), .vcc(vcc), .vssx(vssx), .clk(clk), .d(net76), .s(rst));
i0sfan018aa1d12x5  fan07 ( .ob(net34), .vcc(vcc), .vssx(vssx), .clk(clk), .d(net119), .s(rst));
i0sfan018aa1d12x5  fan03 ( .ob(net48), .vcc(vcc), .vssx(vssx), .clk(clk), .d(net39), .s(rst));
i0sfan018aa1d12x5  fan02 ( .ob(net7), .vcc(vcc), .vssx(vssx), .clk(clk), .d(net23), .s(rst));
i0sfan018aa1d12x5  fan01 ( .ob(net17), .vcc(vcc), .vssx(vssx), .clk(clk), .d(net15), .s(rst));
i0sfan018aa1d12x5  fan00 ( .ob(net3), .vcc(vcc), .vssx(vssx), .clk(clk), .d(cin), .s(rst));
i0snor002aa1n06x5  nor00 ( .o1(cin), .vcc(vcc), .vssx(vssx), .a(net62), .b(net6));
i0smxn022aa1n09x5  mxn02 ( .o1(net119), .vcc(vcc), .vssx(vssx), .a(net126), .b(net132), .sa(sync[1]));
i0smxn022aa1n09x5  mxn01 ( .o1(net132), .vcc(vcc), .vssx(vssx), .a(c2), .b(c1), .sa(sync[0]));
i0smxn022aa1n09x5  mxn00 ( .o1(net126), .vcc(vcc), .vssx(vssx), .a(c4), .b(c3), .sa(sync[0]));
i0stilo00aa1n02x5  tih1 ( .o(t1), .vcc(vcc), .vssx(vssx));
i0scbf000aa1n24x5  cbf03 ( .clkout(ckd), .vcc(vcc), .vssx(vssx), .clk(net163));
i0scbf000aa1n24x5  cbf02 ( .clkout(ckc), .vcc(vcc), .vssx(vssx), .clk(net155));
i0scbf000aa1n24x5  cbf01 ( .clkout(ckb), .vcc(vcc), .vssx(vssx), .clk(net142));
i0scbf000aa1n24x5  cbf00 ( .clkout(cka), .vcc(vcc), .vssx(vssx), .clk(net136));
i0szvss00aa1n00x5  \ss[2]  ( .vcc(vcc), .vssx(vssx));
i0szvss00aa1n00x5  \ss[1]  ( .vcc(vcc), .vssx(vssx));
i0szvss00aa1n00x5  \ss[0]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[9]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[8]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[7]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[6]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[5]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[4]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[3]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[2]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[1]  ( .vcc(vcc), .vssx(vssx));
i0szdcf33ua1n04x5  \f224[0]  ( .vcc(vcc), .vssx(vssx));
i0scinv00aa1d30x5  ckinv2 ( .clkout(clk), .vcc(vcc), .vssx(vssx), .clk(clkb));
i0sinv000aa1n12x5  inv00 ( .o1(rst), .vcc(vcc), .vssx(vssx), .a(rstb));
i0scilma5aa1n06x5  cilb3 ( .clkout(net163), .vcc(vcc), .vssx(vssx), .clk(clk), .en(net1), .te(te));
i0scilma5aa1n06x5  cilb2 ( .clkout(net155), .vcc(vcc), .vssx(vssx), .clk(clk), .en(net2), .te(te));
i0scilma5aa1n06x5  cilb1 ( .clkout(net142), .vcc(vcc), .vssx(vssx), .clk(clk), .en(net4), .te(te));
i0scilma5aa1n06x5  cilb0 ( .clkout(net136), .vcc(vcc), .vssx(vssx), .clk(clk), .en(net5), .te(te));
i0scilma5aa1n06x5  cilb4 ( .clkout(net9), .vcc(vcc), .vssx(vssx), .clk(t1), .en(t1), .te(t1));
i0snand02aa1n08x5  nand0 ( .o1(net62), .vcc(vcc), .vssx(vssx), .a(rstb), .b(net7));
i0snand02aa1n08x5  nand1 ( .o1(net6), .vcc(vcc), .vssx(vssx), .a(net3), .b(net17));
i0scinv00aa1n08x5  cinv23 ( .clkout(net112), .vcc(vcc), .vssx(vssx), .clk(net74));
i0scinv00aa1n08x5  cinv22 ( .clkout(net74), .vcc(vcc), .vssx(vssx), .clk(net1));
i0scinv00aa1n08x5  cinv21 ( .clkout(net1), .vcc(vcc), .vssx(vssx), .clk(net73));
i0scinv00aa1n08x5  cinv18 ( .clkout(net78), .vcc(vcc), .vssx(vssx), .clk(net70));
i0scinv00aa1n08x5  cinv19 ( .clkout(net70), .vcc(vcc), .vssx(vssx), .clk(net2));
i0scinv00aa1n08x5  cinv20 ( .clkout(net2), .vcc(vcc), .vssx(vssx), .clk(net80));
i0scinv00aa1n08x5  cinv17 ( .clkout(net77), .vcc(vcc), .vssx(vssx), .clk(net65));
i0scinv00aa1n08x5  cinv16 ( .clkout(net65), .vcc(vcc), .vssx(vssx), .clk(net4));
i0scinv00aa1n08x5  cinv15 ( .clkout(net4), .vcc(vcc), .vssx(vssx), .clk(net61));
i0scinv00aa1n08x5  cinv14 ( .clkout(net76), .vcc(vcc), .vssx(vssx), .clk(net56));
i0scinv00aa1n08x5  cinv13 ( .clkout(net56), .vcc(vcc), .vssx(vssx), .clk(net5));
i0scinv00aa1n08x5  cinv12 ( .clkout(net5), .vcc(vcc), .vssx(vssx), .clk(net34));
i0scinv00aa1n08x5  cinv11 ( .clkout(net53), .vcc(vcc), .vssx(vssx), .clk(net47));
i0scinv00aa1n08x5  cinv10 ( .clkout(net47), .vcc(vcc), .vssx(vssx), .clk(c4));
i0scinv00aa1n08x5  cinv9 ( .clkout(c4), .vcc(vcc), .vssx(vssx), .clk(net48));
i0scinv00aa1n08x5  cinv6 ( .clkout(net39), .vcc(vcc), .vssx(vssx), .clk(net32));
i0scinv00aa1n08x5  cinv7 ( .clkout(net32), .vcc(vcc), .vssx(vssx), .clk(c3));
i0scinv00aa1n08x5  cinv8 ( .clkout(c3), .vcc(vcc), .vssx(vssx), .clk(net7));
i0scinv00aa1n08x5  cinv5 ( .clkout(net23), .vcc(vcc), .vssx(vssx), .clk(net13));
i0scinv00aa1n08x5  cinv4 ( .clkout(net13), .vcc(vcc), .vssx(vssx), .clk(c2));
i0scinv00aa1n08x5  cinv3 ( .clkout(c2), .vcc(vcc), .vssx(vssx), .clk(net17));
i0scinv00aa1n08x5  cinv2 ( .clkout(net15), .vcc(vcc), .vssx(vssx), .clk(c1b));
i0scinv00aa1n08x5  cinv1 ( .clkout(c1b), .vcc(vcc), .vssx(vssx), .clk(c1));
i0scinv00aa1n08x5  cinv0 ( .clkout(c1), .vcc(vcc), .vssx(vssx), .clk(net3));
i0szdcf33ua1n08x5  f228 ( .vcc(vcc), .vssx(vssx));
i0scinv00aa1n24x4  cbf04 ( .clkout(ckdiv4), .vcc(vcc), .vssx(vssx), .clk(c1b));

endmodule


// End HDL models
