module \lib.comb.add.kpg.KPG_ADD_32-Lfalse-R.1000  (
input \A[0] ,
input \A[10] ,
input \A[11] ,
input \A[12] ,
input \A[13] ,
input \A[14] ,
input \A[15] ,
input \A[16] ,
input \A[17] ,
input \A[18] ,
input \A[19] ,
input \A[1] ,
input \A[20] ,
input \A[21] ,
input \A[22] ,
input \A[23] ,
input \A[24] ,
input \A[25] ,
input \A[26] ,
input \A[27] ,
input \A[28] ,
input \A[29] ,
input \A[2] ,
input \A[30] ,
input \A[31] ,
input \A[3] ,
input \A[4] ,
input \A[5] ,
input \A[6] ,
input \A[7] ,
input \A[8] ,
input \A[9] ,
input \B[0] ,
input \B[10] ,
input \B[11] ,
input \B[12] ,
input \B[13] ,
input \B[14] ,
input \B[15] ,
input \B[16] ,
input \B[17] ,
input \B[18] ,
input \B[19] ,
input \B[1] ,
input \B[20] ,
input \B[21] ,
input \B[22] ,
input \B[23] ,
input \B[24] ,
input \B[25] ,
input \B[26] ,
input \B[27] ,
input \B[28] ,
input \B[29] ,
input \B[2] ,
input \B[30] ,
input \B[31] ,
input \B[3] ,
input \B[4] ,
input \B[5] ,
input \B[6] ,
input \B[7] ,
input \B[8] ,
input \B[9] ,
input GND,
output \S[0] ,
output \S[10] ,
output \S[11] ,
output \S[12] ,
output \S[13] ,
output \S[14] ,
output \S[15] ,
output \S[16] ,
output \S[17] ,
output \S[18] ,
output \S[19] ,
output \S[1] ,
output \S[20] ,
output \S[21] ,
output \S[22] ,
output \S[23] ,
output \S[24] ,
output \S[25] ,
output \S[26] ,
output \S[27] ,
output \S[28] ,
output \S[29] ,
output \S[2] ,
output \S[30] ,
output \S[31] ,
output \S[3] ,
output \S[4] ,
output \S[5] ,
output \S[6] ,
output \S[7] ,
output \S[8] ,
output \S[9] ,
input Vdd
`ifdef INTEL_EMULATION
,
input S_CLK
`endif
);
wire \_kg2[0][0][1] ;
wire \_kg2[0][1][1] ;
wire \_kg2[1][2][0] ;
wire \_kg2[1][2][1] ;
wire \_kg2[2][3][0] ;
wire \_kg2[2][3][1] ;
wire \_kg2[3][4][0] ;
wire \_kg2[3][4][1] ;
wire \_kg2[4][5][0] ;
wire \_kg2[4][5][1] ;
wire \_kg2[5][6][0] ;
wire \_kg2[5][6][1] ;
wire \_kg4[0][0][1] ;
wire \_kg4[0][1][1] ;
wire \_kg4[0][2][1] ;
wire \_kg4[0][3][1] ;
wire \_kg4[0][4][1] ;
wire \_kg4[0][5][1] ;
wire \_kg4[0][6][1] ;
wire \kg3[0][0][1] ;
wire \kg3[0][1][1] ;
wire \kg3[0][2][1] ;
wire \kg3[0][3][1] ;
wire \kg3[1][4][0] ;
wire \kg3[1][4][1] ;
wire \kg3[2][5][0] ;
wire \kg3[2][5][1] ;
wire \kg3[3][6][0] ;
wire \kg3[3][6][1] ;
wire \kg3b[0][2][1] ;
wire \kg[0][0][1] ;
wire \kg[1][1][0] ;
wire \kg[1][1][1] ;
wire \kg[2][2][0] ;
wire \kg[2][2][1] ;
wire \kg[3][3][0] ;
wire \kg[3][3][1] ;
wire \kg[4][4][0] ;
wire \kg[4][4][1] ;
wire \kg[5][5][0] ;
wire \kg[5][5][1] ;
wire \kg[6][6][0] ;
wire \kg[6][6][1] ;
wire \nyb[0]._c2 ;
wire \nyb[0]._kg[0][1][1] ;
wire \nyb[0]._kg[2][3][0] ;
wire \nyb[0]._kg[2][3][1] ;
wire \nyb[0].c[0] ;
wire \nyb[0].c[1] ;
wire \nyb[0].c[3] ;
wire \nyb[0].kg[0][0][0] ;
wire \nyb[0].kg[1][1][0] ;
wire \nyb[0].kg[1][1][1] ;
wire \nyb[0].kg[2][2][0] ;
wire \nyb[0].kg[2][2][1] ;
wire \nyb[0].kg[3][3][0] ;
wire \nyb[0].kg[3][3][1] ;
wire \nyb[1]._kg[0][0][0] ;
wire \nyb[1]._kg[0][0][1] ;
wire \nyb[1]._kg[0][1][0] ;
wire \nyb[1]._kg[0][1][1] ;
wire \nyb[1]._kg[2][3][0] ;
wire \nyb[1]._kg[2][3][1] ;
wire \nyb[1].c[1] ;
wire \nyb[1].c[2] ;
wire \nyb[1].c[3] ;
wire \nyb[1].kg[0][0][0] ;
wire \nyb[1].kg[0][0][1] ;
wire \nyb[1].kg[1][1][0] ;
wire \nyb[1].kg[1][1][1] ;
wire \nyb[1].kg[2][2][0] ;
wire \nyb[1].kg[2][2][1] ;
wire \nyb[1].kg[3][3][0] ;
wire \nyb[1].kg[3][3][1] ;
wire \nyb[2]._kg[0][0][0] ;
wire \nyb[2]._kg[0][0][1] ;
wire \nyb[2]._kg[0][1][0] ;
wire \nyb[2]._kg[0][1][1] ;
wire \nyb[2]._kg[2][3][0] ;
wire \nyb[2]._kg[2][3][1] ;
wire \nyb[2].c[1] ;
wire \nyb[2].c[2] ;
wire \nyb[2].c[3] ;
wire \nyb[2].kg[0][0][0] ;
wire \nyb[2].kg[0][0][1] ;
wire \nyb[2].kg[1][1][0] ;
wire \nyb[2].kg[1][1][1] ;
wire \nyb[2].kg[2][2][0] ;
wire \nyb[2].kg[2][2][1] ;
wire \nyb[2].kg[3][3][0] ;
wire \nyb[2].kg[3][3][1] ;
wire \nyb[3]._kg[0][0][0] ;
wire \nyb[3]._kg[0][0][1] ;
wire \nyb[3]._kg[0][1][0] ;
wire \nyb[3]._kg[0][1][1] ;
wire \nyb[3]._kg[2][3][0] ;
wire \nyb[3]._kg[2][3][1] ;
wire \nyb[3].c[1] ;
wire \nyb[3].c[2] ;
wire \nyb[3].c[3] ;
wire \nyb[3].kg[0][0][0] ;
wire \nyb[3].kg[0][0][1] ;
wire \nyb[3].kg[1][1][0] ;
wire \nyb[3].kg[1][1][1] ;
wire \nyb[3].kg[2][2][0] ;
wire \nyb[3].kg[2][2][1] ;
wire \nyb[3].kg[3][3][0] ;
wire \nyb[3].kg[3][3][1] ;
wire \nyb[4]._kg[0][0][0] ;
wire \nyb[4]._kg[0][0][1] ;
wire \nyb[4]._kg[0][1][0] ;
wire \nyb[4]._kg[0][1][1] ;
wire \nyb[4]._kg[2][3][0] ;
wire \nyb[4]._kg[2][3][1] ;
wire \nyb[4].c[1] ;
wire \nyb[4].c[2] ;
wire \nyb[4].c[3] ;
wire \nyb[4].kg[0][0][0] ;
wire \nyb[4].kg[0][0][1] ;
wire \nyb[4].kg[1][1][0] ;
wire \nyb[4].kg[1][1][1] ;
wire \nyb[4].kg[2][2][0] ;
wire \nyb[4].kg[2][2][1] ;
wire \nyb[4].kg[3][3][0] ;
wire \nyb[4].kg[3][3][1] ;
wire \nyb[5]._kg[0][0][0] ;
wire \nyb[5]._kg[0][0][1] ;
wire \nyb[5]._kg[0][1][0] ;
wire \nyb[5]._kg[0][1][1] ;
wire \nyb[5]._kg[2][3][0] ;
wire \nyb[5]._kg[2][3][1] ;
wire \nyb[5].c[1] ;
wire \nyb[5].c[2] ;
wire \nyb[5].c[3] ;
wire \nyb[5].kg[0][0][0] ;
wire \nyb[5].kg[0][0][1] ;
wire \nyb[5].kg[1][1][0] ;
wire \nyb[5].kg[1][1][1] ;
wire \nyb[5].kg[2][2][0] ;
wire \nyb[5].kg[2][2][1] ;
wire \nyb[5].kg[3][3][0] ;
wire \nyb[5].kg[3][3][1] ;
wire \nyb[6]._kg[0][0][0] ;
wire \nyb[6]._kg[0][0][1] ;
wire \nyb[6]._kg[0][1][0] ;
wire \nyb[6]._kg[0][1][1] ;
wire \nyb[6]._kg[2][3][0] ;
wire \nyb[6]._kg[2][3][1] ;
wire \nyb[6].c[1] ;
wire \nyb[6].c[2] ;
wire \nyb[6].c[3] ;
wire \nyb[6].kg[0][0][0] ;
wire \nyb[6].kg[0][0][1] ;
wire \nyb[6].kg[1][1][0] ;
wire \nyb[6].kg[1][1][1] ;
wire \nyb[6].kg[2][2][0] ;
wire \nyb[6].kg[2][2][1] ;
wire \nyb[6].kg[3][3][0] ;
wire \nyb[6].kg[3][3][1] ;
wire \nyb[7]._kg[0][0][0] ;
wire \nyb[7]._kg[0][0][1] ;
wire \nyb[7]._kg[0][1][0] ;
wire \nyb[7]._kg[0][1][1] ;
wire \nyb[7]._kg[0][2][0] ;
wire \nyb[7]._kg[0][2][1] ;
wire \nyb[7]._kg[2][2][0] ;
wire \nyb[7]._kg[2][2][1] ;
wire \nyb[7].c[1] ;
wire \nyb[7].c[2] ;
wire \nyb[7].c[3] ;
wire \nyb[7].kg[0][0][0] ;
wire \nyb[7].kg[0][0][1] ;
wire \nyb[7].kg[0][2][0] ;
wire \nyb[7].kg[0][2][1] ;
wire \nyb[7].kg[1][1][0] ;
wire \nyb[7].kg[1][1][1] ;
wire \nyb[7].kg[2][2][0] ;
wire \nyb[7].kg[2][2][1] ;
wire \nyb[7].kg[3][3][0] ;
wire \nyb[7].kg[3][3][1] ;
wire \nyb[0].sum[0].x ;
wire \nyb[0].sum[1].x ;
wire \nyb[0].sum[2].x ;
wire \nyb[0].sum[3].x ;
wire \nyb[1].sum[0].x ;
wire \nyb[1].sum[1].x ;
wire \nyb[1].sum[2].x ;
wire \nyb[1].sum[3].x ;
wire \nyb[2].sum[0].x ;
wire \nyb[2].sum[1].x ;
wire \nyb[2].sum[2].x ;
wire \nyb[2].sum[3].x ;
wire \nyb[3].sum[0].x ;
wire \nyb[3].sum[1].x ;
wire \nyb[3].sum[2].x ;
wire \nyb[3].sum[3].x ;
wire \nyb[4].sum[0].x ;
wire \nyb[4].sum[1].x ;
wire \nyb[4].sum[2].x ;
wire \nyb[4].sum[3].x ;
wire \nyb[5].sum[0].x ;
wire \nyb[5].sum[1].x ;
wire \nyb[5].sum[2].x ;
wire \nyb[5].sum[3].x ;
wire \nyb[6].sum[0].x ;
wire \nyb[6].sum[1].x ;
wire \nyb[6].sum[2].x ;
wire \nyb[6].sum[3].x ;
wire \nyb[7].sum[0].x ;
wire \nyb[7].sum[1].x ;
wire \nyb[7].sum[2].x ;
wire \nyb[7].sum[3].x ;
i0skpgcmbaa1n02x5 \kpg2[1][2].kg  (
.\A[0] (\kg[1][1][0] ),
.\A[1] (\kg[1][1][1] ),
.\B[0] (\kg[2][2][0] ),
.\B[1] (\kg[2][2][1] ),
.\C[0] (\_kg2[1][2][0] ),
.\C[1] (\_kg2[1][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \kpg2[2][3].kg  (
.\A[0] (\kg[2][2][0] ),
.\A[1] (\kg[2][2][1] ),
.\B[0] (\kg[3][3][0] ),
.\B[1] (\kg[3][3][1] ),
.\C[0] (\_kg2[2][3][0] ),
.\C[1] (\_kg2[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \kpg2[3][4].kg  (
.\A[0] (\kg[3][3][0] ),
.\A[1] (\kg[3][3][1] ),
.\B[0] (\kg[4][4][0] ),
.\B[1] (\kg[4][4][1] ),
.\C[0] (\_kg2[3][4][0] ),
.\C[1] (\_kg2[3][4][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \kpg2[4][5].kg  (
.\A[0] (\kg[4][4][0] ),
.\A[1] (\kg[4][4][1] ),
.\B[0] (\kg[5][5][0] ),
.\B[1] (\kg[5][5][1] ),
.\C[0] (\_kg2[4][5][0] ),
.\C[1] (\_kg2[4][5][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \kpg2[5][6].kg  (
.\A[0] (\kg[5][5][0] ),
.\A[1] (\kg[5][5][1] ),
.\B[0] (\kg[6][6][0] ),
.\B[1] (\kg[6][6][1] ),
.\C[0] (\_kg2[5][6][0] ),
.\C[1] (\_kg2[5][6][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n02x5 \kpg3[1][4].kg.k.aoi  (
.a(\_kg2[3][4][1] ),
.b(\_kg2[1][2][1] ),
.c(\_kg2[3][4][0] ),
.o1(\kg3[1][4][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \kpg3[1][4].kg.g.oai  (
.a(\_kg2[3][4][0] ),
.b(\_kg2[1][2][0] ),
.c(\_kg2[3][4][1] ),
.o1(\kg3[1][4][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n02x5 \kpg3[2][5].kg.k.aoi  (
.a(\_kg2[4][5][1] ),
.b(\_kg2[2][3][1] ),
.c(\_kg2[4][5][0] ),
.o1(\kg3[2][5][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \kpg3[2][5].kg.g.oai  (
.a(\_kg2[4][5][0] ),
.b(\_kg2[2][3][0] ),
.c(\_kg2[4][5][1] ),
.o1(\kg3[2][5][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n02x5 \kpg3[3][6].kg.k.aoi  (
.a(\_kg2[5][6][1] ),
.b(\_kg2[3][4][1] ),
.c(\_kg2[5][6][0] ),
.o1(\kg3[3][6][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \kpg3[3][6].kg.g.oai  (
.a(\_kg2[5][6][0] ),
.b(\_kg2[3][4][0] ),
.c(\_kg2[5][6][1] ),
.o1(\kg3[3][6][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[0].enc[0]  (
.A(\A[0] ),
.B(\B[0] ),
.\KG[0] (\nyb[0].kg[0][0][0] ),
.\KG[1] (\nyb[0].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[0].enc[1]  (
.A(\A[1] ),
.B(\B[1] ),
.\KG[0] (\nyb[0].kg[1][1][0] ),
.\KG[1] (\nyb[0].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[0].enc[2]  (
.A(\A[2] ),
.B(\B[2] ),
.\KG[0] (\nyb[0].kg[2][2][0] ),
.\KG[1] (\nyb[0].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[0].enc[3]  (
.A(\A[3] ),
.B(\B[3] ),
.\KG[0] (\nyb[0].kg[3][3][0] ),
.\KG[1] (\nyb[0].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[0].kpg[2][3].kg  (
.\A[0] (\nyb[0].kg[2][2][0] ),
.\A[1] (\nyb[0].kg[2][2][1] ),
.\B[0] (\nyb[0].kg[3][3][0] ),
.\B[1] (\nyb[0].kg[3][3][1] ),
.\C[0] (\nyb[0]._kg[2][3][0] ),
.\C[1] (\nyb[0]._kg[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0stilo00aa1n02x5 \nyb[0].tie.tie  (
.o(\nyb[0].c[0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n01x1 \nyb[0].invc2.inv  (
.a(\nyb[0]._kg[0][1][1] ),
.o1(\nyb[0]._c2 ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n01x1 \nyb[0].ctop.oai  (
.a(\nyb[0].kg[2][2][1] ),
.b(\nyb[0].kg[2][2][0] ),
.c(\nyb[0]._c2 ),
.o1(\nyb[0].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \nyb[0].hlf[0][1].g.oai  (
.a(\nyb[0].kg[1][1][1] ),
.b(\nyb[0].kg[1][1][0] ),
.c(\nyb[0].c[1] ),
.o1(\nyb[0]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n02x5 \nyb[0].hlf[0][3].g.aoi  (
.a(\nyb[0]._kg[2][3][1] ),
.b(\nyb[0]._kg[2][3][0] ),
.c(\nyb[0]._kg[0][1][1] ),
.o1(\kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[0].sum[0].nob.nob  (
.a(\nyb[0].c[1] ),
.b(\nyb[0].kg[0][0][0] ),
.out0(\nyb[0].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[0].sum[0].sum.xor  (
.a(\nyb[0].sum[0].x ),
.b(\nyb[0].c[0] ),
.out0(\S[0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[0].sum[1].nob.nob  (
.a(\nyb[0].kg[1][1][1] ),
.b(\nyb[0].kg[1][1][0] ),
.out0(\nyb[0].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[0].sum[1].sum.xnor  (
.a(\nyb[0].sum[1].x ),
.b(\nyb[0].c[1] ),
.out0(\S[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[0].sum[2].nob.nob  (
.a(\nyb[0].kg[2][2][1] ),
.b(\nyb[0].kg[2][2][0] ),
.out0(\nyb[0].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[0].sum[2].sum.xor  (
.a(\nyb[0].sum[2].x ),
.b(\nyb[0]._kg[0][1][1] ),
.out0(\S[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[0].sum[3].nob.nob  (
.a(\nyb[0].kg[3][3][1] ),
.b(\nyb[0].kg[3][3][0] ),
.out0(\nyb[0].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[0].sum[3].sum.xor  (
.a(\nyb[0].sum[3].x ),
.b(\nyb[0].c[3] ),
.out0(\S[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[1].enc[0]  (
.A(\A[4] ),
.B(\B[4] ),
.\KG[0] (\nyb[1].kg[0][0][0] ),
.\KG[1] (\nyb[1].kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[1].enc[1]  (
.A(\A[5] ),
.B(\B[5] ),
.\KG[0] (\nyb[1].kg[1][1][0] ),
.\KG[1] (\nyb[1].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[1].enc[2]  (
.A(\A[6] ),
.B(\B[6] ),
.\KG[0] (\nyb[1].kg[2][2][0] ),
.\KG[1] (\nyb[1].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[1].enc[3]  (
.A(\A[7] ),
.B(\B[7] ),
.\KG[0] (\nyb[1].kg[3][3][0] ),
.\KG[1] (\nyb[1].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[1].kpg[0][1].kg  (
.\A[0] (\nyb[1].kg[0][0][0] ),
.\A[1] (\nyb[1].kg[0][0][1] ),
.\B[0] (\nyb[1].kg[1][1][0] ),
.\B[1] (\nyb[1].kg[1][1][1] ),
.\C[0] (\nyb[1]._kg[0][1][0] ),
.\C[1] (\nyb[1]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[1].kpg[2][3].kg  (
.\A[0] (\nyb[1].kg[2][2][0] ),
.\A[1] (\nyb[1].kg[2][2][1] ),
.\B[0] (\nyb[1].kg[3][3][0] ),
.\B[1] (\nyb[1].kg[3][3][1] ),
.\C[0] (\nyb[1]._kg[2][3][0] ),
.\C[1] (\nyb[1]._kg[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[1].kpg[0][3].kg  (
.\A[0] (\nyb[1]._kg[0][1][1] ),
.\A[1] (\nyb[1]._kg[0][1][0] ),
.\B[0] (\nyb[1]._kg[2][3][1] ),
.\B[1] (\nyb[1]._kg[2][3][0] ),
.\C[0] (\kg[1][1][1] ),
.\C[1] (\kg[1][1][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[1].inv[0][0]  (
.\L[0] (\nyb[1].kg[0][0][0] ),
.\L[1] (\nyb[1].kg[0][0][1] ),
.\R[0] (\nyb[1]._kg[0][0][0] ),
.\R[1] (\nyb[1]._kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[1].cin[1].aoi  (
.a(\nyb[1]._kg[0][0][1] ),
.b(\nyb[1]._kg[0][0][0] ),
.c(\_kg4[0][0][1] ),
.o1(\nyb[1].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[1].cin[2].aoi  (
.a(\nyb[1]._kg[0][1][1] ),
.b(\nyb[1]._kg[0][1][0] ),
.c(\_kg4[0][0][1] ),
.o1(\nyb[1].c[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n01x1 \nyb[1].ctop.oai  (
.a(\nyb[1].kg[2][2][1] ),
.b(\nyb[1].kg[2][2][0] ),
.c(\nyb[1].c[2] ),
.o1(\nyb[1].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[1].sum[0].nob.nob  (
.a(\nyb[1].kg[0][0][1] ),
.b(\nyb[1].kg[0][0][0] ),
.out0(\nyb[1].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[1].sum[0].sum.xor  (
.a(\nyb[1].sum[0].x ),
.b(\_kg4[0][0][1] ),
.out0(\S[4] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[1].sum[1].nob.nob  (
.a(\nyb[1].kg[1][1][1] ),
.b(\nyb[1].kg[1][1][0] ),
.out0(\nyb[1].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[1].sum[1].sum.xnor  (
.a(\nyb[1].sum[1].x ),
.b(\nyb[1].c[1] ),
.out0(\S[5] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[1].sum[2].nob.nob  (
.a(\nyb[1].kg[2][2][1] ),
.b(\nyb[1].kg[2][2][0] ),
.out0(\nyb[1].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[1].sum[2].sum.xnor  (
.a(\nyb[1].sum[2].x ),
.b(\nyb[1].c[2] ),
.out0(\S[6] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[1].sum[3].nob.nob  (
.a(\nyb[1].kg[3][3][1] ),
.b(\nyb[1].kg[3][3][0] ),
.out0(\nyb[1].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[1].sum[3].sum.xor  (
.a(\nyb[1].sum[3].x ),
.b(\nyb[1].c[3] ),
.out0(\S[7] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[2].enc[0]  (
.A(\A[8] ),
.B(\B[8] ),
.\KG[0] (\nyb[2].kg[0][0][0] ),
.\KG[1] (\nyb[2].kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[2].enc[1]  (
.A(\A[9] ),
.B(\B[9] ),
.\KG[0] (\nyb[2].kg[1][1][0] ),
.\KG[1] (\nyb[2].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[2].enc[2]  (
.A(\A[10] ),
.B(\B[10] ),
.\KG[0] (\nyb[2].kg[2][2][0] ),
.\KG[1] (\nyb[2].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[2].enc[3]  (
.A(\A[11] ),
.B(\B[11] ),
.\KG[0] (\nyb[2].kg[3][3][0] ),
.\KG[1] (\nyb[2].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[2].kpg[0][1].kg  (
.\A[0] (\nyb[2].kg[0][0][0] ),
.\A[1] (\nyb[2].kg[0][0][1] ),
.\B[0] (\nyb[2].kg[1][1][0] ),
.\B[1] (\nyb[2].kg[1][1][1] ),
.\C[0] (\nyb[2]._kg[0][1][0] ),
.\C[1] (\nyb[2]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[2].kpg[2][3].kg  (
.\A[0] (\nyb[2].kg[2][2][0] ),
.\A[1] (\nyb[2].kg[2][2][1] ),
.\B[0] (\nyb[2].kg[3][3][0] ),
.\B[1] (\nyb[2].kg[3][3][1] ),
.\C[0] (\nyb[2]._kg[2][3][0] ),
.\C[1] (\nyb[2]._kg[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[2].kpg[0][3].kg  (
.\A[0] (\nyb[2]._kg[0][1][1] ),
.\A[1] (\nyb[2]._kg[0][1][0] ),
.\B[0] (\nyb[2]._kg[2][3][1] ),
.\B[1] (\nyb[2]._kg[2][3][0] ),
.\C[0] (\kg[2][2][1] ),
.\C[1] (\kg[2][2][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[2].inv[0][0]  (
.\L[0] (\nyb[2].kg[0][0][0] ),
.\L[1] (\nyb[2].kg[0][0][1] ),
.\R[0] (\nyb[2]._kg[0][0][0] ),
.\R[1] (\nyb[2]._kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[2].cin[1].aoi  (
.a(\nyb[2]._kg[0][0][1] ),
.b(\nyb[2]._kg[0][0][0] ),
.c(\_kg4[0][1][1] ),
.o1(\nyb[2].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[2].cin[2].aoi  (
.a(\nyb[2]._kg[0][1][1] ),
.b(\nyb[2]._kg[0][1][0] ),
.c(\_kg4[0][1][1] ),
.o1(\nyb[2].c[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n01x1 \nyb[2].ctop.oai  (
.a(\nyb[2].kg[2][2][1] ),
.b(\nyb[2].kg[2][2][0] ),
.c(\nyb[2].c[2] ),
.o1(\nyb[2].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[2].sum[0].nob.nob  (
.a(\nyb[2].kg[0][0][1] ),
.b(\nyb[2].kg[0][0][0] ),
.out0(\nyb[2].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[2].sum[0].sum.xor  (
.a(\nyb[2].sum[0].x ),
.b(\_kg4[0][1][1] ),
.out0(\S[8] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[2].sum[1].nob.nob  (
.a(\nyb[2].kg[1][1][1] ),
.b(\nyb[2].kg[1][1][0] ),
.out0(\nyb[2].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[2].sum[1].sum.xnor  (
.a(\nyb[2].sum[1].x ),
.b(\nyb[2].c[1] ),
.out0(\S[9] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[2].sum[2].nob.nob  (
.a(\nyb[2].kg[2][2][1] ),
.b(\nyb[2].kg[2][2][0] ),
.out0(\nyb[2].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[2].sum[2].sum.xnor  (
.a(\nyb[2].sum[2].x ),
.b(\nyb[2].c[2] ),
.out0(\S[10] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[2].sum[3].nob.nob  (
.a(\nyb[2].kg[3][3][1] ),
.b(\nyb[2].kg[3][3][0] ),
.out0(\nyb[2].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[2].sum[3].sum.xor  (
.a(\nyb[2].sum[3].x ),
.b(\nyb[2].c[3] ),
.out0(\S[11] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[3].enc[0]  (
.A(\A[12] ),
.B(\B[12] ),
.\KG[0] (\nyb[3].kg[0][0][0] ),
.\KG[1] (\nyb[3].kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[3].enc[1]  (
.A(\A[13] ),
.B(\B[13] ),
.\KG[0] (\nyb[3].kg[1][1][0] ),
.\KG[1] (\nyb[3].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[3].enc[2]  (
.A(\A[14] ),
.B(\B[14] ),
.\KG[0] (\nyb[3].kg[2][2][0] ),
.\KG[1] (\nyb[3].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[3].enc[3]  (
.A(\A[15] ),
.B(\B[15] ),
.\KG[0] (\nyb[3].kg[3][3][0] ),
.\KG[1] (\nyb[3].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[3].kpg[0][1].kg  (
.\A[0] (\nyb[3].kg[0][0][0] ),
.\A[1] (\nyb[3].kg[0][0][1] ),
.\B[0] (\nyb[3].kg[1][1][0] ),
.\B[1] (\nyb[3].kg[1][1][1] ),
.\C[0] (\nyb[3]._kg[0][1][0] ),
.\C[1] (\nyb[3]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[3].kpg[2][3].kg  (
.\A[0] (\nyb[3].kg[2][2][0] ),
.\A[1] (\nyb[3].kg[2][2][1] ),
.\B[0] (\nyb[3].kg[3][3][0] ),
.\B[1] (\nyb[3].kg[3][3][1] ),
.\C[0] (\nyb[3]._kg[2][3][0] ),
.\C[1] (\nyb[3]._kg[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[3].kpg[0][3].kg  (
.\A[0] (\nyb[3]._kg[0][1][1] ),
.\A[1] (\nyb[3]._kg[0][1][0] ),
.\B[0] (\nyb[3]._kg[2][3][1] ),
.\B[1] (\nyb[3]._kg[2][3][0] ),
.\C[0] (\kg[3][3][1] ),
.\C[1] (\kg[3][3][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[3].inv[0][0]  (
.\L[0] (\nyb[3].kg[0][0][0] ),
.\L[1] (\nyb[3].kg[0][0][1] ),
.\R[0] (\nyb[3]._kg[0][0][0] ),
.\R[1] (\nyb[3]._kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[3].cin[1].aoi  (
.a(\nyb[3]._kg[0][0][1] ),
.b(\nyb[3]._kg[0][0][0] ),
.c(\_kg4[0][2][1] ),
.o1(\nyb[3].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[3].cin[2].aoi  (
.a(\nyb[3]._kg[0][1][1] ),
.b(\nyb[3]._kg[0][1][0] ),
.c(\_kg4[0][2][1] ),
.o1(\nyb[3].c[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n01x1 \nyb[3].ctop.oai  (
.a(\nyb[3].kg[2][2][1] ),
.b(\nyb[3].kg[2][2][0] ),
.c(\nyb[3].c[2] ),
.o1(\nyb[3].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[3].sum[0].nob.nob  (
.a(\nyb[3].kg[0][0][1] ),
.b(\nyb[3].kg[0][0][0] ),
.out0(\nyb[3].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[3].sum[0].sum.xor  (
.a(\nyb[3].sum[0].x ),
.b(\_kg4[0][2][1] ),
.out0(\S[12] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[3].sum[1].nob.nob  (
.a(\nyb[3].kg[1][1][1] ),
.b(\nyb[3].kg[1][1][0] ),
.out0(\nyb[3].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[3].sum[1].sum.xnor  (
.a(\nyb[3].sum[1].x ),
.b(\nyb[3].c[1] ),
.out0(\S[13] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[3].sum[2].nob.nob  (
.a(\nyb[3].kg[2][2][1] ),
.b(\nyb[3].kg[2][2][0] ),
.out0(\nyb[3].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[3].sum[2].sum.xnor  (
.a(\nyb[3].sum[2].x ),
.b(\nyb[3].c[2] ),
.out0(\S[14] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[3].sum[3].nob.nob  (
.a(\nyb[3].kg[3][3][1] ),
.b(\nyb[3].kg[3][3][0] ),
.out0(\nyb[3].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[3].sum[3].sum.xor  (
.a(\nyb[3].sum[3].x ),
.b(\nyb[3].c[3] ),
.out0(\S[15] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[4].enc[0]  (
.A(\A[16] ),
.B(\B[16] ),
.\KG[0] (\nyb[4].kg[0][0][0] ),
.\KG[1] (\nyb[4].kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[4].enc[1]  (
.A(\A[17] ),
.B(\B[17] ),
.\KG[0] (\nyb[4].kg[1][1][0] ),
.\KG[1] (\nyb[4].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[4].enc[2]  (
.A(\A[18] ),
.B(\B[18] ),
.\KG[0] (\nyb[4].kg[2][2][0] ),
.\KG[1] (\nyb[4].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[4].enc[3]  (
.A(\A[19] ),
.B(\B[19] ),
.\KG[0] (\nyb[4].kg[3][3][0] ),
.\KG[1] (\nyb[4].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[4].kpg[0][1].kg  (
.\A[0] (\nyb[4].kg[0][0][0] ),
.\A[1] (\nyb[4].kg[0][0][1] ),
.\B[0] (\nyb[4].kg[1][1][0] ),
.\B[1] (\nyb[4].kg[1][1][1] ),
.\C[0] (\nyb[4]._kg[0][1][0] ),
.\C[1] (\nyb[4]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[4].kpg[2][3].kg  (
.\A[0] (\nyb[4].kg[2][2][0] ),
.\A[1] (\nyb[4].kg[2][2][1] ),
.\B[0] (\nyb[4].kg[3][3][0] ),
.\B[1] (\nyb[4].kg[3][3][1] ),
.\C[0] (\nyb[4]._kg[2][3][0] ),
.\C[1] (\nyb[4]._kg[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[4].kpg[0][3].kg  (
.\A[0] (\nyb[4]._kg[0][1][1] ),
.\A[1] (\nyb[4]._kg[0][1][0] ),
.\B[0] (\nyb[4]._kg[2][3][1] ),
.\B[1] (\nyb[4]._kg[2][3][0] ),
.\C[0] (\kg[4][4][1] ),
.\C[1] (\kg[4][4][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[4].inv[0][0]  (
.\L[0] (\nyb[4].kg[0][0][0] ),
.\L[1] (\nyb[4].kg[0][0][1] ),
.\R[0] (\nyb[4]._kg[0][0][0] ),
.\R[1] (\nyb[4]._kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[4].cin[1].aoi  (
.a(\nyb[4]._kg[0][0][1] ),
.b(\nyb[4]._kg[0][0][0] ),
.c(\_kg4[0][3][1] ),
.o1(\nyb[4].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[4].cin[2].aoi  (
.a(\nyb[4]._kg[0][1][1] ),
.b(\nyb[4]._kg[0][1][0] ),
.c(\_kg4[0][3][1] ),
.o1(\nyb[4].c[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n01x1 \nyb[4].ctop.oai  (
.a(\nyb[4].kg[2][2][1] ),
.b(\nyb[4].kg[2][2][0] ),
.c(\nyb[4].c[2] ),
.o1(\nyb[4].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[4].sum[0].nob.nob  (
.a(\nyb[4].kg[0][0][1] ),
.b(\nyb[4].kg[0][0][0] ),
.out0(\nyb[4].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[4].sum[0].sum.xor  (
.a(\nyb[4].sum[0].x ),
.b(\_kg4[0][3][1] ),
.out0(\S[16] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[4].sum[1].nob.nob  (
.a(\nyb[4].kg[1][1][1] ),
.b(\nyb[4].kg[1][1][0] ),
.out0(\nyb[4].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[4].sum[1].sum.xnor  (
.a(\nyb[4].sum[1].x ),
.b(\nyb[4].c[1] ),
.out0(\S[17] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[4].sum[2].nob.nob  (
.a(\nyb[4].kg[2][2][1] ),
.b(\nyb[4].kg[2][2][0] ),
.out0(\nyb[4].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[4].sum[2].sum.xnor  (
.a(\nyb[4].sum[2].x ),
.b(\nyb[4].c[2] ),
.out0(\S[18] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[4].sum[3].nob.nob  (
.a(\nyb[4].kg[3][3][1] ),
.b(\nyb[4].kg[3][3][0] ),
.out0(\nyb[4].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[4].sum[3].sum.xor  (
.a(\nyb[4].sum[3].x ),
.b(\nyb[4].c[3] ),
.out0(\S[19] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[5].enc[0]  (
.A(\A[20] ),
.B(\B[20] ),
.\KG[0] (\nyb[5].kg[0][0][0] ),
.\KG[1] (\nyb[5].kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[5].enc[1]  (
.A(\A[21] ),
.B(\B[21] ),
.\KG[0] (\nyb[5].kg[1][1][0] ),
.\KG[1] (\nyb[5].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[5].enc[2]  (
.A(\A[22] ),
.B(\B[22] ),
.\KG[0] (\nyb[5].kg[2][2][0] ),
.\KG[1] (\nyb[5].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[5].enc[3]  (
.A(\A[23] ),
.B(\B[23] ),
.\KG[0] (\nyb[5].kg[3][3][0] ),
.\KG[1] (\nyb[5].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[5].kpg[0][1].kg  (
.\A[0] (\nyb[5].kg[0][0][0] ),
.\A[1] (\nyb[5].kg[0][0][1] ),
.\B[0] (\nyb[5].kg[1][1][0] ),
.\B[1] (\nyb[5].kg[1][1][1] ),
.\C[0] (\nyb[5]._kg[0][1][0] ),
.\C[1] (\nyb[5]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[5].kpg[2][3].kg  (
.\A[0] (\nyb[5].kg[2][2][0] ),
.\A[1] (\nyb[5].kg[2][2][1] ),
.\B[0] (\nyb[5].kg[3][3][0] ),
.\B[1] (\nyb[5].kg[3][3][1] ),
.\C[0] (\nyb[5]._kg[2][3][0] ),
.\C[1] (\nyb[5]._kg[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[5].kpg[0][3].kg  (
.\A[0] (\nyb[5]._kg[0][1][1] ),
.\A[1] (\nyb[5]._kg[0][1][0] ),
.\B[0] (\nyb[5]._kg[2][3][1] ),
.\B[1] (\nyb[5]._kg[2][3][0] ),
.\C[0] (\kg[5][5][1] ),
.\C[1] (\kg[5][5][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[5].inv[0][0]  (
.\L[0] (\nyb[5].kg[0][0][0] ),
.\L[1] (\nyb[5].kg[0][0][1] ),
.\R[0] (\nyb[5]._kg[0][0][0] ),
.\R[1] (\nyb[5]._kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[5].cin[1].aoi  (
.a(\nyb[5]._kg[0][0][1] ),
.b(\nyb[5]._kg[0][0][0] ),
.c(\_kg4[0][4][1] ),
.o1(\nyb[5].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[5].cin[2].aoi  (
.a(\nyb[5]._kg[0][1][1] ),
.b(\nyb[5]._kg[0][1][0] ),
.c(\_kg4[0][4][1] ),
.o1(\nyb[5].c[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n01x1 \nyb[5].ctop.oai  (
.a(\nyb[5].kg[2][2][1] ),
.b(\nyb[5].kg[2][2][0] ),
.c(\nyb[5].c[2] ),
.o1(\nyb[5].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[5].sum[0].nob.nob  (
.a(\nyb[5].kg[0][0][1] ),
.b(\nyb[5].kg[0][0][0] ),
.out0(\nyb[5].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[5].sum[0].sum.xor  (
.a(\nyb[5].sum[0].x ),
.b(\_kg4[0][4][1] ),
.out0(\S[20] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[5].sum[1].nob.nob  (
.a(\nyb[5].kg[1][1][1] ),
.b(\nyb[5].kg[1][1][0] ),
.out0(\nyb[5].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[5].sum[1].sum.xnor  (
.a(\nyb[5].sum[1].x ),
.b(\nyb[5].c[1] ),
.out0(\S[21] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[5].sum[2].nob.nob  (
.a(\nyb[5].kg[2][2][1] ),
.b(\nyb[5].kg[2][2][0] ),
.out0(\nyb[5].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[5].sum[2].sum.xnor  (
.a(\nyb[5].sum[2].x ),
.b(\nyb[5].c[2] ),
.out0(\S[22] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[5].sum[3].nob.nob  (
.a(\nyb[5].kg[3][3][1] ),
.b(\nyb[5].kg[3][3][0] ),
.out0(\nyb[5].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[5].sum[3].sum.xor  (
.a(\nyb[5].sum[3].x ),
.b(\nyb[5].c[3] ),
.out0(\S[23] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[6].enc[0]  (
.A(\A[24] ),
.B(\B[24] ),
.\KG[0] (\nyb[6].kg[0][0][0] ),
.\KG[1] (\nyb[6].kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[6].enc[1]  (
.A(\A[25] ),
.B(\B[25] ),
.\KG[0] (\nyb[6].kg[1][1][0] ),
.\KG[1] (\nyb[6].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[6].enc[2]  (
.A(\A[26] ),
.B(\B[26] ),
.\KG[0] (\nyb[6].kg[2][2][0] ),
.\KG[1] (\nyb[6].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[6].enc[3]  (
.A(\A[27] ),
.B(\B[27] ),
.\KG[0] (\nyb[6].kg[3][3][0] ),
.\KG[1] (\nyb[6].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[6].kpg[0][1].kg  (
.\A[0] (\nyb[6].kg[0][0][0] ),
.\A[1] (\nyb[6].kg[0][0][1] ),
.\B[0] (\nyb[6].kg[1][1][0] ),
.\B[1] (\nyb[6].kg[1][1][1] ),
.\C[0] (\nyb[6]._kg[0][1][0] ),
.\C[1] (\nyb[6]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[6].kpg[2][3].kg  (
.\A[0] (\nyb[6].kg[2][2][0] ),
.\A[1] (\nyb[6].kg[2][2][1] ),
.\B[0] (\nyb[6].kg[3][3][0] ),
.\B[1] (\nyb[6].kg[3][3][1] ),
.\C[0] (\nyb[6]._kg[2][3][0] ),
.\C[1] (\nyb[6]._kg[2][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[6].kpg[0][3].kg  (
.\A[0] (\nyb[6]._kg[0][1][1] ),
.\A[1] (\nyb[6]._kg[0][1][0] ),
.\B[0] (\nyb[6]._kg[2][3][1] ),
.\B[1] (\nyb[6]._kg[2][3][0] ),
.\C[0] (\kg[6][6][1] ),
.\C[1] (\kg[6][6][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[6].inv[0][0]  (
.\L[0] (\nyb[6].kg[0][0][0] ),
.\L[1] (\nyb[6].kg[0][0][1] ),
.\R[0] (\nyb[6]._kg[0][0][0] ),
.\R[1] (\nyb[6]._kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[6].cin[1].aoi  (
.a(\nyb[6]._kg[0][0][1] ),
.b(\nyb[6]._kg[0][0][0] ),
.c(\_kg4[0][5][1] ),
.o1(\nyb[6].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[6].cin[2].aoi  (
.a(\nyb[6]._kg[0][1][1] ),
.b(\nyb[6]._kg[0][1][0] ),
.c(\_kg4[0][5][1] ),
.o1(\nyb[6].c[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n01x1 \nyb[6].ctop.oai  (
.a(\nyb[6].kg[2][2][1] ),
.b(\nyb[6].kg[2][2][0] ),
.c(\nyb[6].c[2] ),
.o1(\nyb[6].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[6].sum[0].nob.nob  (
.a(\nyb[6].kg[0][0][1] ),
.b(\nyb[6].kg[0][0][0] ),
.out0(\nyb[6].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[6].sum[0].sum.xor  (
.a(\nyb[6].sum[0].x ),
.b(\_kg4[0][5][1] ),
.out0(\S[24] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[6].sum[1].nob.nob  (
.a(\nyb[6].kg[1][1][1] ),
.b(\nyb[6].kg[1][1][0] ),
.out0(\nyb[6].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[6].sum[1].sum.xnor  (
.a(\nyb[6].sum[1].x ),
.b(\nyb[6].c[1] ),
.out0(\S[25] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[6].sum[2].nob.nob  (
.a(\nyb[6].kg[2][2][1] ),
.b(\nyb[6].kg[2][2][0] ),
.out0(\nyb[6].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[6].sum[2].sum.xnor  (
.a(\nyb[6].sum[2].x ),
.b(\nyb[6].c[2] ),
.out0(\S[26] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[6].sum[3].nob.nob  (
.a(\nyb[6].kg[3][3][1] ),
.b(\nyb[6].kg[3][3][0] ),
.out0(\nyb[6].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[6].sum[3].sum.xor  (
.a(\nyb[6].sum[3].x ),
.b(\nyb[6].c[3] ),
.out0(\S[27] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[7].enc[0]  (
.A(\A[28] ),
.B(\B[28] ),
.\KG[0] (\nyb[7].kg[0][0][0] ),
.\KG[1] (\nyb[7].kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[7].enc[1]  (
.A(\A[29] ),
.B(\B[29] ),
.\KG[0] (\nyb[7].kg[1][1][0] ),
.\KG[1] (\nyb[7].kg[1][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[7].enc[2]  (
.A(\A[30] ),
.B(\B[30] ),
.\KG[0] (\nyb[7].kg[2][2][0] ),
.\KG[1] (\nyb[7].kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgencaa1n02x5 \nyb[7].enc[3]  (
.A(\A[31] ),
.B(\B[31] ),
.\KG[0] (\nyb[7].kg[3][3][0] ),
.\KG[1] (\nyb[7].kg[3][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[7].kpg[0][1].kg  (
.\A[0] (\nyb[7].kg[0][0][0] ),
.\A[1] (\nyb[7].kg[0][0][1] ),
.\B[0] (\nyb[7].kg[1][1][0] ),
.\B[1] (\nyb[7].kg[1][1][1] ),
.\C[0] (\nyb[7]._kg[0][1][0] ),
.\C[1] (\nyb[7]._kg[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[7].inv[0][0]  (
.\L[0] (\nyb[7].kg[0][0][0] ),
.\L[1] (\nyb[7].kg[0][0][1] ),
.\R[0] (\nyb[7]._kg[0][0][0] ),
.\R[1] (\nyb[7]._kg[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[7].inv[2][2]  (
.\L[0] (\nyb[7].kg[2][2][0] ),
.\L[1] (\nyb[7].kg[2][2][1] ),
.\R[0] (\nyb[7]._kg[2][2][0] ),
.\R[1] (\nyb[7]._kg[2][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpgcmbaa1n02x5 \nyb[7].kpg[0][2].kg  (
.\A[0] (\nyb[7]._kg[0][1][1] ),
.\A[1] (\nyb[7]._kg[0][1][0] ),
.\B[0] (\nyb[7]._kg[2][2][1] ),
.\B[1] (\nyb[7]._kg[2][2][0] ),
.\C[0] (\nyb[7].kg[0][2][1] ),
.\C[1] (\nyb[7].kg[0][2][0] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0skpginvaa1n01x1 \nyb[7].inv[0][2]  (
.\L[0] (\nyb[7].kg[0][2][0] ),
.\L[1] (\nyb[7].kg[0][2][1] ),
.\R[0] (\nyb[7]._kg[0][2][0] ),
.\R[1] (\nyb[7]._kg[0][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[7].cin[1].aoi  (
.a(\nyb[7]._kg[0][0][1] ),
.b(\nyb[7]._kg[0][0][0] ),
.c(\_kg4[0][6][1] ),
.o1(\nyb[7].c[1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[7].cin[2].aoi  (
.a(\nyb[7]._kg[0][1][1] ),
.b(\nyb[7]._kg[0][1][0] ),
.c(\_kg4[0][6][1] ),
.o1(\nyb[7].c[2] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n01x1 \nyb[7].cin[3].aoi  (
.a(\nyb[7]._kg[0][2][1] ),
.b(\nyb[7]._kg[0][2][0] ),
.c(\_kg4[0][6][1] ),
.o1(\nyb[7].c[3] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[7].sum[0].nob.nob  (
.a(\nyb[7].kg[0][0][1] ),
.b(\nyb[7].kg[0][0][0] ),
.out0(\nyb[7].sum[0].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxor002aa1n01x1 \nyb[7].sum[0].sum.xor  (
.a(\nyb[7].sum[0].x ),
.b(\_kg4[0][6][1] ),
.out0(\S[28] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[7].sum[1].nob.nob  (
.a(\nyb[7].kg[1][1][1] ),
.b(\nyb[7].kg[1][1][0] ),
.out0(\nyb[7].sum[1].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[7].sum[1].sum.xnor  (
.a(\nyb[7].sum[1].x ),
.b(\nyb[7].c[1] ),
.out0(\S[29] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[7].sum[2].nob.nob  (
.a(\nyb[7].kg[2][2][1] ),
.b(\nyb[7].kg[2][2][0] ),
.out0(\nyb[7].sum[2].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[7].sum[2].sum.xnor  (
.a(\nyb[7].sum[2].x ),
.b(\nyb[7].c[2] ),
.out0(\S[30] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0snorb02aa1n01x1 \nyb[7].sum[3].nob.nob  (
.a(\nyb[7].kg[3][3][1] ),
.b(\nyb[7].kg[3][3][0] ),
.out0(\nyb[7].sum[3].x ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sxnr002aa1n01x1 \nyb[7].sum[3].sum.xnor  (
.a(\nyb[7].sum[3].x ),
.b(\nyb[7].c[3] ),
.out0(\S[31] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n02x5 \inh2[0][0].inv.inv  (
.a(\kg[0][0][1] ),
.o1(\_kg2[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \hlf2[0][1].g.oai  (
.a(\kg[1][1][1] ),
.b(\kg[1][1][0] ),
.c(\kg[0][0][1] ),
.o1(\_kg2[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n02x5 \inh3[0][0].inv.inv  (
.a(\_kg2[0][0][1] ),
.o1(\kg3[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n02x5 \inh3[0][1].inv.inv  (
.a(\_kg2[0][1][1] ),
.o1(\kg3[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n03x5 \hlf3[0][2].g.aoi  (
.a(\_kg2[1][2][1] ),
.b(\_kg2[1][2][0] ),
.c(\_kg2[0][0][1] ),
.o1(\kg3[0][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n02x5 \hlf3b[0][2].g.aoi  (
.a(\_kg2[1][2][1] ),
.b(\_kg2[1][2][0] ),
.c(\_kg2[0][0][1] ),
.o1(\kg3b[0][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0saoi012aa1n02x5 \hlf3[0][3].g.aoi  (
.a(\_kg2[2][3][1] ),
.b(\_kg2[2][3][0] ),
.c(\_kg2[0][1][1] ),
.o1(\kg3[0][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n02x5 \inh4[0][0].inv.inv  (
.a(\kg3[0][0][1] ),
.o1(\_kg4[0][0][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n02x5 \inh4[0][1].inv.inv  (
.a(\kg3[0][1][1] ),
.o1(\_kg4[0][1][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n02x5 \inh4[0][2].inv.inv  (
.a(\kg3b[0][2][1] ),
.o1(\_kg4[0][2][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0sinv000aa1n02x5 \inh4[0][3].inv.inv  (
.a(\kg3[0][3][1] ),
.o1(\_kg4[0][3][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \hlf4[0][4].g.oai  (
.a(\kg3[1][4][1] ),
.b(\kg3[1][4][0] ),
.c(\kg3[0][0][1] ),
.o1(\_kg4[0][4][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \hlf4[0][5].g.oai  (
.a(\kg3[2][5][1] ),
.b(\kg3[2][5][0] ),
.c(\kg3[0][1][1] ),
.o1(\_kg4[0][5][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
i0soai012aa1n02x5 \hlf4[0][6].g.oai  (
.a(\kg3[3][6][1] ),
.b(\kg3[3][6][0] ),
.c(\kg3[0][2][1] ),
.o1(\_kg4[0][6][1] ),
.vcc(Vdd),
.vssx(GND)
`ifdef INTEL_EMULATION
,
.S_CLK(S_CLK)
`endif
);
endmodule        // module with subcells


