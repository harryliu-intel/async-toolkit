
///
///  INTEL CONFIDENTIAL
///
///  Copyright 2015 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
//-----------------------------------------------------------------------------
// Title         : 
// Project       : Fox River
//-----------------------------------------------------------------------------
// File          : fxr_10b_ecc_gen.sv
// Author        : neuman.paul  <paul.neuman@intel.com>
// Created       : 17.06.2015  d.m.y
//-----------------------------------------------------------------------------
// Description :  !)b ECC Gen
//-----------------------------------------------------------------------------
// Intel Proprietary
// Copyright (C) 2015 Intel Corporation
// All Rights Reserved
//------------------------------------------------------------------------------

module fxr_10b_ecc_gen (
  input  logic [255:0]     data,  
  output logic [9:0]       chk
);

  logic [9:0]              syndromein;
  logic [255:0]            datain;
  logic [9:0]              part_sum_0;
  logic [9:0]              part_sum_1;
  logic [9:0]              part_sum_2;
  logic [9:0]              part_sum_3;
  logic [9:0]              part_sum_4;
  logic [9:0]              part_sum_5;
  logic [9:0]              syndrome_x;
  
  assign syndromein = 10'b0 ; 
  
  assign datain = data ;
  
  assign part_sum_0[0] =  (datain[  0] ^ datain[  5] ^ datain[ 10] ^ datain[ 14] ^ datain[ 15] ^ datain[ 18] ^ 
                           datain[ 20] ^ datain[ 21] ^ datain[ 22] ^ datain[ 23] ^ datain[ 24] ^ datain[ 25] ^ 
                           datain[ 30] ^ datain[ 34] ^ datain[ 35] ^ datain[ 38] ^ datain[ 40] ^ datain[ 41]);

  assign part_sum_0[1] =  (datain[  1] ^ datain[  6] ^ datain[ 10] ^ datain[ 11] ^ datain[ 16] ^ datain[ 19] ^ 
                           datain[ 20] ^ datain[ 21] ^ datain[ 22] ^ datain[ 26] ^ datain[ 27] ^ datain[ 28] ^ 
                           datain[ 30] ^ datain[ 31] ^ datain[ 36] ^ datain[ 39] ^ datain[ 41] ^ datain[ 44]);

  assign part_sum_0[2] =  (datain[  2] ^ datain[  7] ^ datain[ 11] ^ datain[ 12] ^ datain[ 15] ^ datain[ 17] ^ 
                           datain[ 20] ^ datain[ 23] ^ datain[ 24] ^ datain[ 26] ^ datain[ 27] ^ datain[ 29] ^ 
                           datain[ 31] ^ datain[ 32] ^ datain[ 35] ^ datain[ 37] ^ datain[ 40]);

  assign part_sum_0[3] =  (datain[  3] ^ datain[  8] ^ datain[ 12] ^ datain[ 13] ^ datain[ 16] ^ datain[ 18] ^ 
                           datain[ 21] ^ datain[ 23] ^ datain[ 25] ^ datain[ 26] ^ datain[ 28] ^ datain[ 29] ^ 
                           datain[ 32] ^ datain[ 33] ^ datain[ 36] ^ datain[ 38] ^ datain[ 40]);

  assign part_sum_0[4] =  (datain[  4] ^ datain[  9] ^ datain[ 13] ^ datain[ 14] ^ datain[ 17] ^ datain[ 19] ^ 
                           datain[ 22] ^ datain[ 24] ^ datain[ 25] ^ datain[ 27] ^ datain[ 28] ^ datain[ 29] ^ 
                           datain[ 33] ^ datain[ 34] ^ datain[ 37] ^ datain[ 39] ^ datain[ 41]);

  assign part_sum_0[5] =  (datain[  0] ^ datain[  4] ^ datain[  5] ^ datain[  8] ^ datain[ 10] ^ datain[ 15] ^ 
                           datain[ 20] ^ datain[ 24] ^ datain[ 25] ^ datain[ 28] ^ datain[ 30] ^ datain[ 31] ^ 
                           datain[ 32] ^ datain[ 33] ^ datain[ 34] ^ datain[ 35] ^ datain[ 42] ^ datain[ 43]);

  assign part_sum_0[6] =  (datain[  0] ^ datain[  1] ^ datain[  6] ^ datain[  9] ^ datain[ 11] ^ datain[ 16] ^ 
                           datain[ 20] ^ datain[ 21] ^ datain[ 26] ^ datain[ 29] ^ datain[ 30] ^ datain[ 31] ^ 
                           datain[ 32] ^ datain[ 36] ^ datain[ 37] ^ datain[ 38] ^ datain[ 43] ^ datain[ 44]);

  assign part_sum_0[7] =  (datain[  1] ^ datain[  2] ^ datain[  5] ^ datain[  7] ^ datain[ 12] ^ datain[ 17] ^ 
                           datain[ 21] ^ datain[ 22] ^ datain[ 25] ^ datain[ 27] ^ datain[ 30] ^ datain[ 33] ^ 
                           datain[ 34] ^ datain[ 36] ^ datain[ 37] ^ datain[ 39] ^ datain[ 42] ^ datain[ 44]);

  assign part_sum_0[8] =  (datain[  2] ^ datain[  3] ^ datain[  6] ^ datain[  8] ^ datain[ 13] ^ datain[ 18] ^ 
                           datain[ 22] ^ datain[ 23] ^ datain[ 26] ^ datain[ 28] ^ datain[ 31] ^ datain[ 33] ^ 
                           datain[ 35] ^ datain[ 36] ^ datain[ 38] ^ datain[ 39] ^ datain[ 42] ^ datain[ 44]);

  assign part_sum_0[9] =  (datain[  3] ^ datain[  4] ^ datain[  7] ^ datain[  9] ^ datain[ 14] ^ datain[ 19] ^ 
                           datain[ 23] ^ datain[ 24] ^ datain[ 27] ^ datain[ 29] ^ datain[ 32] ^ datain[ 34] ^ 
                           datain[ 35] ^ datain[ 37] ^ datain[ 38] ^ datain[ 39] ^ datain[ 43] ^ datain[ 44]);

  assign part_sum_1[0] =  (datain[ 49] ^ datain[ 54] ^ datain[ 55] ^ datain[ 59] ^ datain[ 60] ^ datain[ 63] ^ 
                           datain[ 65] ^ datain[ 66] ^ datain[ 67] ^ datain[ 68] ^ datain[ 69] ^ datain[ 70] ^ 
                           datain[ 78] ^ datain[ 79] ^ datain[ 82] ^ datain[ 84] ^ datain[ 85]);

  assign part_sum_1[1] =  (datain[ 45] ^ datain[ 50] ^ datain[ 55] ^ datain[ 56] ^ datain[ 61] ^ datain[ 64] ^ 
                           datain[ 65] ^ datain[ 66] ^ datain[ 67] ^ datain[ 71] ^ datain[ 72] ^ datain[ 73] ^ 
                           datain[ 75] ^ datain[ 79] ^ datain[ 80] ^ datain[ 83] ^ datain[ 85]);

  assign part_sum_1[2] =  (datain[ 46] ^ datain[ 51] ^ datain[ 56] ^ datain[ 57] ^ datain[ 60] ^ datain[ 62] ^ 
                           datain[ 65] ^ datain[ 68] ^ datain[ 69] ^ datain[ 71] ^ datain[ 72] ^ datain[ 74] ^ 
                           datain[ 75] ^ datain[ 76] ^ datain[ 81] ^ datain[ 84] ^ datain[ 86]);

  assign part_sum_1[3] =  (datain[ 47] ^ datain[ 52] ^ datain[ 57] ^ datain[ 58] ^ datain[ 61] ^ datain[ 63] ^ 
                           datain[ 66] ^ datain[ 68] ^ datain[ 70] ^ datain[ 71] ^ datain[ 73] ^ datain[ 74] ^ 
                           datain[ 76] ^ datain[ 77] ^ datain[ 80] ^ datain[ 82] ^ datain[ 85] ^ datain[ 86]);

  assign part_sum_1[4] =  (datain[ 48] ^ datain[ 53] ^ datain[ 58] ^ datain[ 59] ^ datain[ 62] ^ datain[ 64] ^ 
                           datain[ 67] ^ datain[ 69] ^ datain[ 70] ^ datain[ 72] ^ datain[ 73] ^ datain[ 74] ^ 
                           datain[ 77] ^ datain[ 78] ^ datain[ 81] ^ datain[ 83] ^ datain[ 86] ^ datain[ 89]);

  assign part_sum_1[5] =  (datain[ 45] ^ datain[ 49] ^ datain[ 50] ^ datain[ 53] ^ datain[ 59] ^ datain[ 64] ^ 
                           datain[ 68] ^ datain[ 69] ^ datain[ 72] ^ datain[ 74] ^ datain[ 75] ^ datain[ 76] ^ 
                           datain[ 77] ^ datain[ 78] ^ datain[ 79] ^ datain[ 80] ^ datain[ 87] ^ datain[ 89]);

  assign part_sum_1[6] =  (datain[ 45] ^ datain[ 46] ^ datain[ 51] ^ datain[ 54] ^ datain[ 55] ^ datain[ 60] ^ 
                           datain[ 65] ^ datain[ 69] ^ datain[ 70] ^ datain[ 73] ^ datain[ 75] ^ datain[ 76] ^ 
                           datain[ 77] ^ datain[ 81] ^ datain[ 82] ^ datain[ 83] ^ datain[ 87] ^ datain[ 89]);

  assign part_sum_1[7] =  (datain[ 46] ^ datain[ 47] ^ datain[ 50] ^ datain[ 52] ^ datain[ 56] ^ datain[ 61] ^ 
                           datain[ 65] ^ datain[ 66] ^ datain[ 71] ^ datain[ 74] ^ datain[ 75] ^ datain[ 78] ^ 
                           datain[ 79] ^ datain[ 81] ^ datain[ 82] ^ datain[ 84] ^ datain[ 88] ^ datain[ 89]);

  assign part_sum_1[8] =  (datain[ 47] ^ datain[ 48] ^ datain[ 51] ^ datain[ 53] ^ datain[ 57] ^ datain[ 62] ^ 
                           datain[ 66] ^ datain[ 67] ^ datain[ 70] ^ datain[ 72] ^ datain[ 76] ^ datain[ 78] ^ 
                           datain[ 80] ^ datain[ 81] ^ datain[ 83] ^ datain[ 84] ^ datain[ 87] ^ datain[ 88]);

  assign part_sum_1[9] =  (datain[ 48] ^ datain[ 49] ^ datain[ 52] ^ datain[ 54] ^ datain[ 58] ^ datain[ 63] ^ 
                           datain[ 67] ^ datain[ 68] ^ datain[ 71] ^ datain[ 73] ^ datain[ 77] ^ datain[ 79] ^ 
                           datain[ 80] ^ datain[ 82] ^ datain[ 83] ^ datain[ 84] ^ datain[ 88] ^ datain[ 89]);

  assign part_sum_2[0] =  (datain[ 93] ^ datain[ 98] ^ datain[100] ^ datain[104] ^ datain[105] ^ datain[108] ^ 
                           datain[110] ^ datain[111] ^ datain[112] ^ datain[113] ^ datain[114] ^ datain[115] ^ 
                           datain[122] ^ datain[123] ^ datain[126] ^ datain[128] ^ datain[131]);

  assign part_sum_2[1] =  (datain[ 94] ^ datain[ 99] ^ datain[100] ^ datain[101] ^ datain[106] ^ datain[109] ^ 
                           datain[110] ^ datain[111] ^ datain[112] ^ datain[116] ^ datain[117] ^ datain[118] ^ 
                           datain[123] ^ datain[124] ^ datain[127] ^ datain[129] ^ datain[130] ^ datain[131]);

  assign part_sum_2[2] =  (datain[ 90] ^ datain[ 95] ^ datain[101] ^ datain[102] ^ datain[105] ^ datain[107] ^ 
                           datain[110] ^ datain[113] ^ datain[114] ^ datain[116] ^ datain[117] ^ datain[119] ^ 
                           datain[120] ^ datain[124] ^ datain[125] ^ datain[128] ^ datain[131] ^ datain[134]);

  assign part_sum_2[3] =  (datain[ 91] ^ datain[ 96] ^ datain[102] ^ datain[103] ^ datain[106] ^ datain[108] ^ 
                           datain[111] ^ datain[113] ^ datain[115] ^ datain[116] ^ datain[118] ^ datain[119] ^ 
                           datain[120] ^ datain[121] ^ datain[126] ^ datain[129] ^ datain[130]);

  assign part_sum_2[4] =  (datain[ 92] ^ datain[ 97] ^ datain[103] ^ datain[104] ^ datain[107] ^ datain[109] ^ 
                           datain[112] ^ datain[114] ^ datain[115] ^ datain[117] ^ datain[118] ^ datain[119] ^ 
                           datain[121] ^ datain[122] ^ datain[125] ^ datain[127] ^ datain[130]);

  assign part_sum_2[5] =  (datain[ 90] ^ datain[ 94] ^ datain[ 95] ^ datain[ 98] ^ datain[103] ^ datain[108] ^ 
                           datain[112] ^ datain[113] ^ datain[116] ^ datain[118] ^ datain[120] ^ datain[121] ^ 
                           datain[122] ^ datain[123] ^ datain[124] ^ datain[125] ^ datain[133] ^ datain[134]);

  assign part_sum_2[6] =  (datain[ 90] ^ datain[ 91] ^ datain[ 96] ^ datain[ 99] ^ datain[104] ^ datain[109] ^ 
                           datain[113] ^ datain[114] ^ datain[117] ^ datain[119] ^ datain[120] ^ datain[121] ^ 
                           datain[122] ^ datain[126] ^ datain[127] ^ datain[128] ^ datain[132] ^ datain[133]);

  assign part_sum_2[7] =  (datain[ 91] ^ datain[ 92] ^ datain[ 95] ^ datain[ 97] ^ datain[100] ^ datain[105] ^ 
                           datain[110] ^ datain[114] ^ datain[115] ^ datain[118] ^ datain[120] ^ datain[123] ^ 
                           datain[124] ^ datain[126] ^ datain[127] ^ datain[129] ^ datain[133] ^ datain[134]);

  assign part_sum_2[8] =  (datain[ 92] ^ datain[ 93] ^ datain[ 96] ^ datain[ 98] ^ datain[101] ^ datain[106] ^ 
                           datain[110] ^ datain[111] ^ datain[116] ^ datain[119] ^ datain[121] ^ datain[123] ^ 
                           datain[125] ^ datain[126] ^ datain[128] ^ datain[129] ^ datain[132] ^ datain[134]);

  assign part_sum_2[9] =  (datain[ 93] ^ datain[ 94] ^ datain[ 97] ^ datain[ 99] ^ datain[102] ^ datain[107] ^ 
                           datain[111] ^ datain[112] ^ datain[115] ^ datain[117] ^ datain[122] ^ datain[124] ^ 
                           datain[125] ^ datain[127] ^ datain[128] ^ datain[129] ^ datain[132] ^ datain[134]);

  assign part_sum_3[0] =  (datain[137] ^ datain[142] ^ datain[145] ^ datain[149] ^ datain[150] ^ datain[153] ^ 
                           datain[155] ^ datain[156] ^ datain[157] ^ datain[158] ^ datain[159] ^ datain[160] ^ 
                           datain[166] ^ datain[167] ^ datain[170] ^ datain[172] ^ datain[176] ^ datain[179]);

  assign part_sum_3[1] =  (datain[138] ^ datain[143] ^ datain[145] ^ datain[146] ^ datain[151] ^ datain[154] ^ 
                           datain[155] ^ datain[156] ^ datain[157] ^ datain[161] ^ datain[162] ^ datain[163] ^ 
                           datain[167] ^ datain[168] ^ datain[171] ^ datain[173] ^ datain[175] ^ datain[179]);

  assign part_sum_3[2] =  (datain[139] ^ datain[144] ^ datain[146] ^ datain[147] ^ datain[150] ^ datain[152] ^ 
                           datain[155] ^ datain[158] ^ datain[159] ^ datain[161] ^ datain[162] ^ datain[164] ^ 
                           datain[168] ^ datain[169] ^ datain[172] ^ datain[174] ^ datain[175] ^ datain[179]);

  assign part_sum_3[3] =  (datain[135] ^ datain[140] ^ datain[147] ^ datain[148] ^ datain[151] ^ datain[153] ^ 
                           datain[156] ^ datain[158] ^ datain[160] ^ datain[161] ^ datain[163] ^ datain[164] ^ 
                           datain[165] ^ datain[169] ^ datain[170] ^ datain[173] ^ datain[176] ^ datain[179]);

  assign part_sum_3[4] =  (datain[136] ^ datain[141] ^ datain[148] ^ datain[149] ^ datain[152] ^ datain[154] ^ 
                           datain[157] ^ datain[159] ^ datain[160] ^ datain[162] ^ datain[163] ^ datain[164] ^ 
                           datain[165] ^ datain[166] ^ datain[171] ^ datain[174] ^ datain[175] ^ datain[176]);

  assign part_sum_3[5] =  (datain[135] ^ datain[139] ^ datain[140] ^ datain[143] ^ datain[147] ^ datain[152] ^ 
                           datain[156] ^ datain[157] ^ datain[160] ^ datain[162] ^ datain[165] ^ datain[166] ^ 
                           datain[167] ^ datain[168] ^ datain[169] ^ datain[170] ^ datain[178]);

  assign part_sum_3[6] =  (datain[135] ^ datain[136] ^ datain[141] ^ datain[144] ^ datain[148] ^ datain[153] ^ 
                           datain[157] ^ datain[158] ^ datain[161] ^ datain[163] ^ datain[165] ^ datain[166] ^ 
                           datain[167] ^ datain[171] ^ datain[172] ^ datain[173] ^ datain[177] ^ datain[179]);

  assign part_sum_3[7] =  (datain[136] ^ datain[137] ^ datain[140] ^ datain[142] ^ datain[149] ^ datain[154] ^ 
                           datain[158] ^ datain[159] ^ datain[162] ^ datain[164] ^ datain[165] ^ datain[168] ^ 
                           datain[169] ^ datain[171] ^ datain[172] ^ datain[174] ^ datain[177]);

  assign part_sum_3[8] =  (datain[137] ^ datain[138] ^ datain[141] ^ datain[143] ^ datain[145] ^ datain[150] ^ 
                           datain[155] ^ datain[159] ^ datain[160] ^ datain[163] ^ datain[166] ^ datain[168] ^ 
                           datain[170] ^ datain[171] ^ datain[173] ^ datain[174] ^ datain[178]);

  assign part_sum_3[9] =  (datain[138] ^ datain[139] ^ datain[142] ^ datain[144] ^ datain[146] ^ datain[151] ^ 
                           datain[155] ^ datain[156] ^ datain[161] ^ datain[164] ^ datain[167] ^ datain[169] ^ 
                           datain[170] ^ datain[172] ^ datain[173] ^ datain[174] ^ datain[177] ^ datain[178]);

  assign part_sum_4[0] =  (datain[181] ^ datain[186] ^ datain[190] ^ datain[194] ^ datain[195] ^ datain[198] ^ 
                           datain[200] ^ datain[201] ^ datain[202] ^ datain[203] ^ datain[204] ^ datain[205] ^ 
                           datain[210] ^ datain[211] ^ datain[216] ^ datain[219] ^ datain[220] ^ datain[224]);

  assign part_sum_4[1] =  (datain[182] ^ datain[187] ^ datain[190] ^ datain[191] ^ datain[196] ^ datain[199] ^ 
                           datain[200] ^ datain[201] ^ datain[202] ^ datain[206] ^ datain[207] ^ datain[208] ^ 
                           datain[211] ^ datain[212] ^ datain[215] ^ datain[217] ^ datain[221] ^ datain[224]);

  assign part_sum_4[2] =  (datain[183] ^ datain[188] ^ datain[191] ^ datain[192] ^ datain[195] ^ datain[197] ^ 
                           datain[200] ^ datain[203] ^ datain[204] ^ datain[206] ^ datain[207] ^ datain[209] ^ 
                           datain[212] ^ datain[213] ^ datain[216] ^ datain[218] ^ datain[220] ^ datain[221]);

  assign part_sum_4[3] =  (datain[184] ^ datain[189] ^ datain[192] ^ datain[193] ^ datain[196] ^ datain[198] ^ 
                           datain[201] ^ datain[203] ^ datain[205] ^ datain[206] ^ datain[208] ^ datain[209] ^ 
                           datain[213] ^ datain[214] ^ datain[217] ^ datain[219] ^ datain[221] ^ datain[224]);

  assign part_sum_4[4] =  (datain[180] ^ datain[185] ^ datain[193] ^ datain[194] ^ datain[197] ^ datain[199] ^ 
                           datain[202] ^ datain[204] ^ datain[205] ^ datain[207] ^ datain[208] ^ datain[209] ^ 
                           datain[210] ^ datain[214] ^ datain[215] ^ datain[218] ^ datain[220] ^ datain[224]);

  assign part_sum_4[5] =  (datain[180] ^ datain[184] ^ datain[185] ^ datain[188] ^ datain[191] ^ datain[196] ^ 
                           datain[200] ^ datain[201] ^ datain[206] ^ datain[209] ^ datain[210] ^ datain[211] ^ 
                           datain[212] ^ datain[213] ^ datain[214] ^ datain[215] ^ datain[222]);

  assign part_sum_4[6] =  (datain[180] ^ datain[181] ^ datain[186] ^ datain[189] ^ datain[192] ^ datain[197] ^ 
                           datain[201] ^ datain[202] ^ datain[205] ^ datain[207] ^ datain[210] ^ datain[211] ^ 
                           datain[212] ^ datain[216] ^ datain[217] ^ datain[218] ^ datain[223]);

  assign part_sum_4[7] =  (datain[181] ^ datain[182] ^ datain[185] ^ datain[187] ^ datain[193] ^ datain[198] ^ 
                           datain[202] ^ datain[203] ^ datain[206] ^ datain[208] ^ datain[210] ^ datain[213] ^ 
                           datain[214] ^ datain[216] ^ datain[217] ^ datain[219] ^ datain[222] ^ datain[223]);

  assign part_sum_4[8] =  (datain[182] ^ datain[183] ^ datain[186] ^ datain[188] ^ datain[194] ^ datain[199] ^ 
                           datain[203] ^ datain[204] ^ datain[207] ^ datain[209] ^ datain[211] ^ datain[213] ^ 
                           datain[215] ^ datain[216] ^ datain[218] ^ datain[219] ^ datain[223] ^ datain[224]);

  assign part_sum_4[9] =  (datain[183] ^ datain[184] ^ datain[187] ^ datain[189] ^ datain[190] ^ datain[195] ^ 
                           datain[200] ^ datain[204] ^ datain[205] ^ datain[208] ^ datain[212] ^ datain[214] ^ 
                           datain[215] ^ datain[217] ^ datain[218] ^ datain[219] ^ datain[222]);

  assign part_sum_5[0] =  (datain[225] ^ datain[229] ^ datain[232] ^ datain[240] ^ datain[241] ^ datain[242] ^ 
                           datain[243] ^ datain[244] ^ datain[245] ^ datain[246] ^ datain[247] ^ datain[248] ^ 
                           datain[249] ^ datain[250] ^ datain[251] ^ datain[252] ^ syndromein[0]);

  assign part_sum_5[1] =  (datain[233] ^ datain[240] ^ datain[241] ^ datain[242] ^ datain[243] ^ datain[244] ^ 
                           datain[245] ^ datain[246] ^ datain[247] ^ datain[248] ^ datain[249] ^ datain[250] ^ 
                           datain[253] ^ datain[254] ^ datain[255] ^ syndromein[1]);

  assign part_sum_5[2] =  (datain[226] ^ datain[230] ^ datain[234] ^ datain[238] ^ datain[240] ^ datain[241] ^ 
                           datain[242] ^ datain[243] ^ datain[244] ^ datain[245] ^ datain[246] ^ datain[251] ^ 
                           datain[252] ^ datain[253] ^ datain[254] ^ datain[255] ^ syndromein[2]);

  assign part_sum_5[3] =  (datain[227] ^ datain[231] ^ datain[236] ^ datain[240] ^ datain[241] ^ datain[242] ^ 
                           datain[243] ^ datain[247] ^ datain[248] ^ datain[249] ^ datain[250] ^ datain[251] ^ 
                           datain[252] ^ datain[253] ^ datain[254] ^ datain[255] ^ syndromein[3]);

  assign part_sum_5[4] =  (datain[228] ^ datain[235] ^ datain[237] ^ datain[239] ^ datain[244] ^ datain[245] ^ 
                           datain[246] ^ datain[247] ^ datain[248] ^ datain[249] ^ datain[250] ^ datain[251] ^ 
                           datain[252] ^ datain[253] ^ datain[254] ^ datain[255] ^ syndromein[4]);

  assign part_sum_5[5] =  (datain[225] ^ datain[226] ^ datain[227] ^ datain[228] ^ datain[229] ^ datain[230] ^ 
                           datain[231] ^ datain[232] ^ datain[233] ^ datain[234] ^ datain[235] ^ datain[236] ^ 
                           datain[237] ^ datain[240] ^ datain[244] ^ datain[247] ^ syndromein[5]);

  assign part_sum_5[6] =  (datain[225] ^ datain[226] ^ datain[227] ^ datain[228] ^ datain[229] ^ datain[230] ^ 
                           datain[231] ^ datain[232] ^ datain[233] ^ datain[234] ^ datain[235] ^ datain[238] ^ 
                           datain[239] ^ datain[245] ^ datain[248] ^ syndromein[6]);

  assign part_sum_5[7] =  (datain[225] ^ datain[226] ^ datain[227] ^ datain[228] ^ datain[229] ^ datain[230] ^ 
                           datain[231] ^ datain[236] ^ datain[237] ^ datain[238] ^ datain[239] ^ datain[241] ^ 
                           datain[246] ^ datain[249] ^ datain[253] ^ syndromein[7]);

  assign part_sum_5[8] =  (datain[225] ^ datain[226] ^ datain[227] ^ datain[228] ^ datain[232] ^ datain[233] ^ 
                           datain[234] ^ datain[235] ^ datain[236] ^ datain[237] ^ datain[238] ^ datain[239] ^ 
                           datain[242] ^ datain[251] ^ datain[254] ^ syndromein[8]);

  assign part_sum_5[9] =  (datain[229] ^ datain[230] ^ datain[231] ^ datain[232] ^ datain[233] ^ datain[234] ^ 
                           datain[235] ^ datain[236] ^ datain[237] ^ datain[238] ^ datain[239] ^ datain[243] ^ 
                           datain[250] ^ datain[252] ^ datain[255] ^ syndromein[9]);

  assign syndrome_x =  part_sum_0 ^ part_sum_1 ^ part_sum_2 ^ part_sum_3 ^ part_sum_4 ^ part_sum_5 ;
  
  assign chk =  syndrome_x ;
  
/*  
  logic       bit0 ;
  logic       bit1 ;
  logic       bit2 ;
  logic       bit3 ;
  logic       bit4 ;
  logic       bit5 ;
  logic       bit6 ;
  logic       bit7 ;
  logic       bit8 ;
  logic       bit9 ;
   
  assign bit0 = (data[0]^data[5]^data[10]^data[14]^data[15]^data[18]^data[20]^data[21]^data[22]^data[23]^data[24]^data[25]^data[30]^data[34]^data[35]^data[38]^data[40]^data[41]^data[49]^data[54]^data[55]^data[59]^data[60]^data[63]^data[65]^data[66]^data[67]^data[68]^data[69]^data[70]^data[78]^data[79]^data[82]^data[84]^data[85]^data[93]^data[98]^data[100]^data[104]^data[105]^data[108]^data[110]^data[111]^data[112]^data[113]^data[114]^data[115]^data[122]^data[123]^data[126]^data[128]^data[131]^data[137]^data[142]^data[145]^data[149]^data[150]^data[153]^data[155]^data[156]^data[157]^data[158]^data[159]^data[160]^data[166]^data[167]^data[170]^data[172]^data[176]^data[179]^data[181]^data[186]^data[190]^data[194]^data[195]^data[198]^data[200]^data[201]^data[202]^data[203]^data[204]^data[205]^data[210]^data[211]^data[216]^data[219]^data[220]^data[224]^data[225]^data[229]^data[232]^data[240]^data[241]^data[242]^data[243]^data[244]^data[245]^data[246]^data[247]^data[248]^data[249]^data[250]^data[251]^data[252]);
  assign bit1 = (data[1]^data[6]^data[10]^data[11]^data[16]^data[19]^data[20]^data[21]^data[22]^data[26]^data[27]^data[28]^data[30]^data[31]^data[36]^data[39]^data[41]^data[44]^data[45]^data[50]^data[55]^data[56]^data[61]^data[64]^data[65]^data[66]^data[67]^data[71]^data[72]^data[73]^data[75]^data[79]^data[80]^data[83]^data[85]^data[94]^data[99]^data[100]^data[101]^data[106]^data[109]^data[110]^data[111]^data[112]^data[116]^data[117]^data[118]^data[123]^data[124]^data[127]^data[129]^data[130]^data[131]^data[138]^data[143]^data[145]^data[146]^data[151]^data[154]^data[155]^data[156]^data[157]^data[161]^data[162]^data[163]^data[167]^data[168]^data[171]^data[173]^data[175]^data[179]^data[182]^data[187]^data[190]^data[191]^data[196]^data[199]^data[200]^data[201]^data[202]^data[206]^data[207]^data[208]^data[211]^data[212]^data[215]^data[217]^data[221]^data[224]^data[233]^data[240]^data[241]^data[242]^data[243]^data[244]^data[245]^data[246]^data[247]^data[248]^data[249]^data[250]^data[ 253]^data[254]^data[255]);
  assign bit2 = (data[2]^data[7]^data[11]^data[12]^data[15]^data[17]^data[20]^data[23]^data[24]^data[26]^data[27]^data[29]^data[31]^data[32]^data[35]^data[37]^data[40]^data[46]^data[51]^data[56]^data[57]^data[60]^data[62]^data[65]^data[68]^data[69]^data[71]^data[72]^data[74]^data[75]^data[76]^data[81]^data[84]^data[86]^data[90]^data[95]^data[101]^data[102]^data[105]^data[107]^data[110]^data[113]^data[114]^data[116]^data[117]^data[119]^data[120]^data[124]^data[125]^data[128]^data[131]^data[134]^data[139]^data[144]^data[146]^data[147]^data[150]^data[152]^data[155]^data[158]^data[159]^data[161]^data[162]^data[164]^data[168]^data[169]^data[172]^data[174]^data[175]^data[179]^data[183]^data[188]^data[191]^data[192]^data[195]^data[197]^data[200]^data[203]^data[204]^data[206]^data[207]^data[209]^data[212]^data[213]^data[216]^data[218]^data[220]^data[221]^data[226]^data[230]^data[234]^data[238]^data[240]^data[241]^data[242]^data[243]^data[244]^data[245]^data[246]^data[251]^data[252]^data[253]^data[254]^data[255]);
  assign bit3 = (data[3]^data[8]^data[12]^data[13]^data[16]^data[18]^data[21]^data[23]^data[25]^data[26]^data[28]^data[29]^data[32]^data[33]^data[36]^data[38]^data[40]^data[47]^data[52]^data[57]^data[58]^data[61]^data[63]^data[ 66]^data[68]^data[70]^data[71]^data[73]^data[74]^data[76]^data[77]^data[80]^data[82]^data[85]^data[86]^data[91]^data[96]^data[102]^data[103]^data[106]^data[108]^data[111]^data[113]^data[115]^data[116]^data[118]^data[119]^data[120]^data[121]^data[126]^data[129]^data[130]^data[135]^data[140]^data[147]^data[148]^data[151]^data[153]^data[156]^data[158]^data[160]^data[161]^data[163]^data[164]^data[165]^data[169]^data[170]^data[173]^data[176]^data[179]^data[184]^data[189]^data[192]^data[193]^data[196]^data[198]^data[201]^data[203]^data[205]^data[206]^data[208]^data[209]^data[213]^data[214]^data[217]^data[219]^data[221]^data[224]^data[227]^data[231]^data[236]^data[240]^data[241]^data[242]^data[243]^data[247]^data[248]^data[249]^data[250]^data[251]^data[252]^data[253]^data[254]^data[255]);
  assign bit4 = (data[4]^data[9]^data[13]^data[14]^data[17]^data[19]^data[ 22]^data[24]^data[25]^data[27]^data[28]^data[29]^data[33]^data[34]^data[37]^data[39]^data[41]^data[48]^data[53]^data[58]^data[59]^data[62]^data[64]^data[67]^data[69]^data[70]^data[72]^data[73]^data[74]^data[77]^data[78]^data[81]^data[83]^data[86]^data[89]^data[92]^data[97]^data[103]^data[104]^data[107]^data[109]^data[112]^data[114]^data[115]^data[117]^data[118]^data[119]^data[121]^data[122]^data[125]^data[127]^data[130]^data[136]^data[141]^data[148]^data[149]^data[152]^data[154]^data[157]^data[159]^data[160]^data[162]^data[163]^data[164]^data[165]^data[166]^data[171]^data[174]^data[175]^data[176]^data[180]^data[185]^data[193]^data[194]^data[197]^data[199]^data[202]^data[204]^data[205]^data[207]^data[208]^data[209]^data[210]^data[214]^data[215]^data[218]^data[220]^data[224]^data[228]^data[235]^data[237]^data[239]^data[244]^data[245]^data[246]^data[247]^data[248]^data[249]^data[250]^data[251]^data[252]^data[253]^data[254]^data[255]);
  assign bit5 = (data[0]^data[4]^data[5]^data[8]^data[10]^data[15]^data[20]^data[24]^data[25]^data[28]^data[30]^data[31]^data[32]^data[33]^data[34]^data[35]^data[42]^data[43]^data[45]^data[49]^data[50]^data[53]^data[59]^data[64]^data[68]^data[69]^data[72]^data[74]^data[75]^data[76]^data[77]^data[78]^data[79]^data[80]^data[87]^data[89]^data[90]^data[94]^data[95]^data[98]^data[103]^data[108]^data[112]^data[113]^data[116]^data[118]^data[120]^data[121]^data[122]^data[123]^data[124]^data[125]^data[133]^data[134]^data[135]^data[139]^data[140]^data[143]^data[147]^data[152]^data[156]^data[157]^data[160]^data[162]^data[165]^data[166]^data[167]^data[168]^data[169]^data[170]^data[178]^data[180]^data[184]^data[185]^data[188]^data[191]^data[196]^data[200]^data[201]^data[206]^data[209]^data[210]^data[211]^data[212]^data[213]^data[214]^data[215]^data[222]^data[225]^data[226]^data[227]^data[228]^data[229]^data[230]^data[231]^data[232]^data[233]^data[234]^data[235]^data[236]^data[237]^data[240]^data[244]^data[247]);
  assign bit6 = (data[0]^data[1]^data[6]^data[9]^data[11]^data[16]^data[20]^data[21]^data[26]^data[29]^data[30]^data[31]^data[32]^data[36]^data[37]^data[38]^data[43]^data[44]^data[45]^data[46]^data[51]^data[54]^data[55]^data[60]^data[65]^data[69]^data[70]^data[73]^data[75]^data[76]^data[77]^data[81]^data[82]^data[83]^data[87]^data[89]^data[90]^data[91]^data[96]^data[99]^data[104]^data[109]^data[113]^data[114]^data[117]^data[119]^data[120]^data[121]^data[122]^data[126]^data[127]^data[128]^data[132]^data[133]^data[135]^data[136]^data[141]^data[144]^data[148]^data[153]^data[157]^data[158]^data[161]^data[163]^data[165]^data[166]^data[167]^data[171]^data[172]^data[173]^data[177]^data[179]^data[180]^data[181]^data[186]^data[189]^data[192]^data[197]^data[201]^data[202]^data[205]^data[207]^data[210]^data[211]^data[212]^data[216]^data[217]^data[218]^data[223]^data[225]^data[226]^data[227]^data[228]^data[229]^data[230]^data[231]^data[232]^data[233]^data[234]^data[235]^data[238]^data[239]^data[245]^data[248]);
  assign bit7 = (data[1]^data[2]^data[5]^data[7]^data[12]^data[17]^data[21]^data[22]^data[25]^data[27]^data[30]^data[33]^data[34]^data[36]^data[37]^data[39]^data[42]^data[44]^data[46]^data[47]^data[50]^data[52]^data[56]^data[61]^data[65]^data[66]^data[71]^data[74]^data[75]^data[78]^data[79]^data[81]^data[82]^data[84]^data[88]^data[89]^data[91]^data[92]^data[95]^data[97]^data[100]^data[105]^data[110]^data[114]^data[115]^data[118]^data[120]^data[123]^data[124]^data[126]^data[127]^data[129]^data[133]^data[134]^data[136]^data[137]^data[140]^data[142]^data[149]^data[154]^data[158]^data[159]^data[162]^data[164]^data[165]^data[168]^data[169]^data[171]^data[172]^data[174]^data[177]^data[181]^data[182]^data[185]^data[187]^data[193]^data[198]^data[202]^data[203]^data[206]^data[208]^data[210]^data[213]^data[214]^data[216]^data[217]^data[219]^data[222]^data[223]^data[225]^data[226]^data[227]^data[228]^data[229]^data[230]^data[231]^data[236]^data[237]^data[238]^data[239]^data[241]^data[246]^data[249]^data[253]);
  assign bit8 = (data[2]^data[3]^data[6]^data[8]^data[13]^data[18]^data[22]^data[23]^data[26]^data[28]^data[31]^data[33]^data[35]^data[36]^data[38]^data[39]^data[42]^data[44]^data[47]^data[48]^data[51]^data[53]^data[57]^data[62]^data[66]^data[67]^data[70]^data[72]^data[76]^data[78]^data[80]^data[81]^data[83]^data[84]^data[87]^data[88]^data[92]^data[93]^data[96]^data[98]^data[101]^data[106]^data[110]^data[111]^data[116]^data[119]^data[121]^data[123]^data[125]^data[126]^data[128]^data[129]^data[132]^data[134]^data[137]^data[138]^data[141]^data[143]^data[145]^data[150]^data[155]^data[159]^data[160]^data[163]^data[166]^data[168]^data[170]^data[171]^data[173]^data[174]^data[178]^data[182]^data[183]^data[186]^data[188]^data[194]^data[199]^data[203]^data[204]^data[207]^data[209]^data[211]^data[213]^data[215]^data[216]^data[218]^data[219]^data[223]^data[224]^data[225]^data[226]^data[227]^data[228]^data[232]^data[233]^data[234]^data[235]^data[236]^data[237]^data[238]^data[239]^data[242]^data[251]^data[254]);
  assign bit9 = (data[3]^data[4]^data[7]^data[9]^data[14]^data[19]^data[23]^data[24]^data[27]^data[29]^data[32]^data[34]^data[35]^data[37]^data[38]^data[39]^data[43]^data[44]^data[48]^data[49]^data[52]^data[54]^data[58]^data[63]^data[67]^data[68]^data[71]^data[73]^data[77]^data[79]^data[80]^data[82]^data[83]^data[84]^data[88]^data[89]^data[93]^data[94]^data[97]^data[99]^data[102]^data[107]^data[111]^data[112]^data[115]^data[117]^data[122]^data[124]^data[125]^data[127]^data[128]^data[129]^data[132]^data[134]^data[138]^data[139]^data[142]^data[144]^data[146]^data[151]^data[155]^data[156]^data[161]^data[164]^data[167]^data[169]^data[170]^data[172]^data[173]^data[174]^data[177]^data[178]^data[183]^data[184]^data[187]^data[189]^data[190]^data[195]^data[200]^data[204]^data[205]^data[208]^data[212]^data[214]^data[215]^data[217]^data[218]^data[219]^data[222]^data[229]^data[230]^data[231]^data[232]^data[233]^data[234]^data[235]^data[236]^data[237]^data[238]^data[239]^data[243]^data[250]^data[252]^data[255]);		   		     
  
  
  assign chk = {bit9,bit8,bit7,bit6,bit5,bit4,bit3,bit2,bit1,bit0} ;
*/
/*  
  logic [255:0] vec0  ;
  logic [255:0] vec1  ;
  logic [255:0] vec2  ;    
  logic [255:0] vec3  ;
  logic [255:0] vec4  ;
  logic [255:0] vec5  ;
  logic [255:0] vec6  ;
  logic [255:0] vec7  ;
  logic [255:0] vec8  ;
  logic [255:0] vec9  ;  
  
  assign vec0 = (2**0)+(2**5)+(2**10)+(2**14)+(2**15)+(2**18)+(2**20)+(2**21)+(2**22)+(2**23)+(2**24)+(2**25)+(2**30)+(2**34)+(2**35)+(2**38)+(2**40)+(2**41)+(2**49)+(2**54)+(2**55)+(2**59)+(2**60)+(2**63)+(2**64)+(2**66)+(2**67)+(2**68)+(2**69)+(2**70)+(2**78)+(2**79)+(2**82)+(2**84)+(2**85)+(2**93)+(2**98)+(2**100)+(2**104)+(2**105)+(2**108)+(2**110)+(2**111)+(2**112)+(2**113)+(2**114)+(2**115)+(2**122)+(2**123)+(2**126)+(2**128)+(2**131)+(2**137)+(2**142)+(2**145)+(2**149)+(2**150)+(2**153)+(2**155)+(2**156)+(2**157)+(2**158)+(2**159)+(2**160)+(2**166)+(2**167)+(2**170)+(2**172)+(2**176)+(2**179)+(2**181)+(2**186)+(2**190)+(2**194)+(2**195)+(2**198)+(2**200)+(2**201)+(2**202)+(2**203)+(2**204)+(2**205)+(2**210)+(2**211)+(2**216)+(2**219)+(2**220)+(2**224)+(2**225)+(2**229)+(2**232)+(2**240)+(2**241)+(2**242)+(2**243)+(2**244)+(2**245)+(2**246)+(2**247)+(2**248)+(2**249)+(2**250)+(2**251)+(2**252);
  assign vec1 = (2**1)+(2**6)+(2**10)+(2**11)+(2**16)+(2**19)+(2**20)+(2**21)+(2**22)+(2**26)+(2**27)+(2**28)+(2**30)+(2**31)+(2**36)+(2**39)+(2**41)+(2**44)+(2**45)+(2**50)+(2**55)+(2**56)+(2**61)+(2**64)+(2**65)+(2**66)+(2**67)+(2**71)+(2**72)+(2**73)+(2**75)+(2**79)+(2**80)+(2**83)+(2**85)+(2**94)+(2**99)+(2**100)+(2**101)+(2**106)+(2**109)+(2**110)+(2**111)+(2**112)+(2**116)+(2**117)+(2**118)+(2**123)+(2**124)+(2**127)+(2**129)+(2**130)+(2**131)+(2**138)+(2**143)+(2**145)+(2**146)+(2**151)+(2**154)+(2**155)+(2**156)+(2**157)+(2**161)+(2**162)+(2**163)+(2**167)+(2**168)+(2**171)+(2**173)+(2**175)+(2**179)+(2**182)+(2**187)+(2**190)+(2**191)+(2**196)+(2**199)+(2**200)+(2**201)+(2**202)+(2**206)+(2**207)+(2**208)+(2**211)+(2**212)+(2**215)+(2**217)+(2**221)+(2**224)+(2**233)+(2**240)+(2**241)+(2**242)+(2**243)+(2**244)+(2**245)+(2**246)+(2**247)+(2**248)+(2**249)+(2**250)+(2** 253)+(2**254)+(2**255);
  assign vec2 = (2**2)+(2**7)+(2**11)+(2**12)+(2**15)+(2**17)+(2**20)+(2**23)+(2**24)+(2**26)+(2**27)+(2**29)+(2**31)+(2**32)+(2**35)+(2**37)+(2**40)+(2**46)+(2**51)+(2**56)+(2**57)+(2**60)+(2**62)+(2**65)+(2**68)+(2**69)+(2**71)+(2**72)+(2**74)+(2**75)+(2**76)+(2**81)+(2**84)+(2**86)+(2**90)+(2**95)+(2**101)+(2**102)+(2**105)+(2**107)+(2**110)+(2**113)+(2**114)+(2**116)+(2**117)+(2**119)+(2**120)+(2**124)+(2**125)+(2**128)+(2**131)+(2**134)+(2**139)+(2**144)+(2**146)+(2**147)+(2**150)+(2**152)+(2**155)+(2**158)+(2**159)+(2**161)+(2**162)+(2**164)+(2**168)+(2**169)+(2**172)+(2**174)+(2**175)+(2**179)+(2**183)+(2**188)+(2**191)+(2**192)+(2**195)+(2**197)+(2**200)+(2**203)+(2**204)+(2**206)+(2**207)+(2**209)+(2**212)+(2**213)+(2**216)+(2**218)+(2**220)+(2**221)+(2**226)+(2**230)+(2**234)+(2**238)+(2**240)+(2**241)+(2**242)+(2**243)+(2**244)+(2**245)+(2**246)+(2**251)+(2**252)+(2**253)+(2**254)+(2**255);
  assign vec3 = (2**3)+(2**8)+(2**12)+(2**13)+(2**16)+(2**18)+(2**21)+(2**23)+(2**25)+(2**26)+(2**28)+(2**29)+(2**32)+(2**33)+(2**36)+(2**38)+(2**40)+(2**47)+(2**52)+(2**57)+(2**58)+(2**61)+(2**63)+(2** 66)+(2**68)+(2**70)+(2**71)+(2**73)+(2**74)+(2**76)+(2**77)+(2**80)+(2**82)+(2**85)+(2**86)+(2**91)+(2**96)+(2**102)+(2**103)+(2**106)+(2**108)+(2**111)+(2**113)+(2**115)+(2**116)+(2**118)+(2**119)+(2**120)+(2**121)+(2**126)+(2**129)+(2**130)+(2**135)+(2**140)+(2**147)+(2**148)+(2**151)+(2**153)+(2**156)+(2**158)+(2**160)+(2**161)+(2**163)+(2**164)+(2**165)+(2**169)+(2**170)+(2**173)+(2**176)+(2**179)+(2**184)+(2**189)+(2**192)+(2**193)+(2**196)+(2**198)+(2**201)+(2**203)+(2**205)+(2**206)+(2**208)+(2**209)+(2**213)+(2**214)+(2**217)+(2**219)+(2**221)+(2**224)+(2**227)+(2**231)+(2**236)+(2**240)+(2**241)+(2**242)+(2**243)+(2**247)+(2**248)+(2**249)+(2**250)+(2**251)+(2**252)+(2**253)+(2**254)+(2**255);
  assign vec4 = (2**4)+(2**9)+(2**13)+(2**14)+(2**17)+(2**19)+(2**22)+(2**24)+(2**25)+(2**27)+(2**28)+(2**29)+(2**33)+(2**34)+(2**37)+(2**39)+(2**41)+(2**48)+(2**53)+(2**58)+(2**59)+(2**62)+(2**64)+(2**67)+(2**69)+(2**70)+(2**72)+(2**73)+(2**74)+(2**77)+(2**78)+(2**81)+(2**83)+(2**86)+(2**89)+(2**92)+(2**97)+(2**103)+(2**104)+(2**107)+(2**109)+(2**112)+(2**114)+(2**115)+(2**117)+(2**118)+(2**119)+(2**121)+(2**122)+(2**125)+(2**127)+(2**130)+(2**136)+(2**141)+(2**148)+(2**149)+(2**152)+(2**154)+(2**157)+(2**159)+(2**160)+(2**162)+(2**163)+(2**164)+(2**165)+(2**166)+(2**171)+(2**174)+(2**175)+(2**176)+(2**180)+(2**185)+(2**193)+(2**194)+(2**197)+(2**199)+(2**202)+(2**204)+(2**205)+(2**207)+(2**208)+(2**209)+(2**210)+(2**214)+(2**215)+(2**218)+(2**220)+(2**224)+(2**228)+(2**235)+(2**237)+(2**239)+(2**244)+(2**245)+(2**246)+(2**247)+(2**248)+(2**249)+(2**250)+(2**251)+(2**252)+(2**253)+(2**254)+(2**255);
  assign vec5 = (2**0)+(2**4)+(2**5)+(2**8)+(2**10)+(2**15)+(2**20)+(2**24)+(2**25)+(2**28)+(2**30)+(2**31)+(2**32)+(2**33)+(2**34)+(2**35)+(2**42)+(2**43)+(2**45)+(2**49)+(2**50)+(2**53)+(2**59)+(2**64)+(2**68)+(2**69)+(2**72)+(2**74)+(2**75)+(2**76)+(2**77)+(2**78)+(2**79)+(2**80)+(2**87)+(2**89)+(2**90)+(2**94)+(2**95)+(2**98)+(2**103)+(2**108)+(2**112)+(2**113)+(2**116)+(2**118)+(2**120)+(2**121)+(2**122)+(2**123)+(2**124)+(2**125)+(2**133)+(2**134)+(2**135)+(2**139)+(2**140)+(2**143)+(2**147)+(2**152)+(2**156)+(2**157)+(2**160)+(2**162)+(2**165)+(2**166)+(2**167)+(2**168)+(2**169)+(2**170)+(2**178)+(2**180)+(2**184)+(2**185)+(2**188)+(2**191)+(2**196)+(2**200)+(2**201)+(2**206)+(2**209)+(2**210)+(2**211)+(2**212)+(2**213)+(2**214)+(2**215)+(2**222)+(2**225)+(2**226)+(2**227)+(2**228)+(2**229)+(2**230)+(2**231)+(2**232)+(2**233)+(2**234)+(2**235)+(2**236)+(2**237)+(2**240)+(2**244)+(2**247);
  assign vec6 = (2**0)+(2**1)+(2**6)+(2**9)+(2**11)+(2**16)+(2**20)+(2**21)+(2**26)+(2**29)+(2**30)+(2**31)+(2**32)+(2**36)+(2**37)+(2**38)+(2**43)+(2**44)+(2**45)+(2**46)+(2**51)+(2**54)+(2**55)+(2**60)+(2**65)+(2**69)+(2**70)+(2**73)+(2**75)+(2**76)+(2**77)+(2**81)+(2**82)+(2**83)+(2**87)+(2**89)+(2**90)+(2**91)+(2**96)+(2**99)+(2**104)+(2**109)+(2**113)+(2**114)+(2**117)+(2**119)+(2**120)+(2**121)+(2**122)+(2**126)+(2**127)+(2**128)+(2**132)+(2**133)+(2**135)+(2**136)+(2**141)+(2**144)+(2**148)+(2**153)+(2**157)+(2**158)+(2**161)+(2**163)+(2**165)+(2**166)+(2**167)+(2**171)+(2**172)+(2**173)+(2**177)+(2**179)+(2**180)+(2**181)+(2**186)+(2**189)+(2**192)+(2**197)+(2**201)+(2**202)+(2**205)+(2**207)+(2**210)+(2**211)+(2**212)+(2**216)+(2**217)+(2**218)+(2**223)+(2**225)+(2**226)+(2**227)+(2**228)+(2**229)+(2**230)+(2**231)+(2**232)+(2**233)+(2**234)+(2**235)+(2**238)+(2**239)+(2**245)+(2**248);
  assign vec7 = (2**1)+(2**2)+(2**5)+(2**7)+(2**12)+(2**17)+(2**21)+(2**22)+(2**25)+(2**27)+(2**30)+(2**33)+(2**34)+(2**36)+(2**37)+(2**39)+(2**42)+(2**44)+(2**46)+(2**47)+(2**50)+(2**52)+(2**56)+(2**61)+(2**65)+(2**66)+(2**71)+(2**74)+(2**75)+(2**78)+(2**79)+(2**81)+(2**82)+(2**84)+(2**88)+(2**89)+(2**91)+(2**92)+(2**95)+(2**97)+(2**100)+(2**105)+(2**110)+(2**114)+(2**115)+(2**118)+(2**120)+(2**123)+(2**124)+(2**126)+(2**127)+(2**129)+(2**133)+(2**134)+(2**136)+(2**137)+(2**140)+(2**142)+(2**149)+(2**154)+(2**158)+(2**159)+(2**162)+(2**164)+(2**165)+(2**168)+(2**169)+(2**171)+(2**172)+(2**174)+(2**177)+(2**181)+(2**182)+(2**185)+(2**187)+(2**193)+(2**198)+(2**202)+(2**203)+(2**206)+(2**208)+(2**210)+(2**213)+(2**214)+(2**216)+(2**217)+(2**219)+(2**222)+(2**223)+(2**225)+(2**226)+(2**227)+(2**228)+(2**229)+(2**230)+(2**231)+(2**236)+(2**237)+(2**238)+(2**239)+(2**241)+(2**246)+(2**249)+(2**253);
  assign vec8 = (2**2)+(2**3)+(2**6)+(2**8)+(2**13)+(2**18)+(2**22)+(2**23)+(2**26)+(2**28)+(2**31)+(2**33)+(2**35)+(2**36)+(2**38)+(2**39)+(2**42)+(2**44)+(2**47)+(2**48)+(2**51)+(2**53)+(2**57)+(2**62)+(2**66)+(2**67)+(2**70)+(2**72)+(2**76)+(2**78)+(2**80)+(2**81)+(2**83)+(2**84)+(2**87)+(2**88)+(2**92)+(2**93)+(2**96)+(2**98)+(2**101)+(2**106)+(2**110)+(2**111)+(2**116)+(2**119)+(2**121)+(2**123)+(2**125)+(2**126)+(2**128)+(2**129)+(2**132)+(2**134)+(2**137)+(2**138)+(2**141)+(2**143)+(2**145)+(2**150)+(2**155)+(2**159)+(2**160)+(2**163)+(2**166)+(2**168)+(2**170)+(2**171)+(2**173)+(2**174)+(2**178)+(2**182)+(2**183)+(2**186)+(2**188)+(2**194)+(2**199)+(2**203)+(2**204)+(2**207)+(2**209)+(2**211)+(2**213)+(2**215)+(2**216)+(2**218)+(2**219)+(2**223)+(2**224)+(2**225)+(2**226)+(2**227)+(2**228)+(2**232)+(2**233)+(2**234)+(2**235)+(2**236)+(2**237)+(2**238)+(2**239)+(2**242)+(2**251)+(2**254);
  assign vec9 = (2**3)+(2**4)+(2**7)+(2**9)+(2**14)+(2**19)+(2**23)+(2**24)+(2**27)+(2**29)+(2**32)+(2**34)+(2**35)+(2**37)+(2**38)+(2**39)+(2**43)+(2**44)+(2**48)+(2**49)+(2**52)+(2**54)+(2**58)+(2**63)+(2**67)+(2**68)+(2**71)+(2**73)+(2**77)+(2**79)+(2**80)+(2**82)+(2**83)+(2**84)+(2**88)+(2**89)+(2**93)+(2**94)+(2**97)+(2**99)+(2**102)+(2**107)+(2**111)+(2**112)+(2**115)+(2**117)+(2**122)+(2**124)+(2**125)+(2**127)+(2**128)+(2**129)+(2**132)+(2**134)+(2**138)+(2**139)+(2**142)+(2**144)+(2**146)+(2**151)+(2**155)+(2**156)+(2**161)+(2**164)+(2**167)+(2**169)+(2**170)+(2**172)+(2**173)+(2**174)+(2**177)+(2**178)+(2**183)+(2**184)+(2**187)+(2**189)+(2**190)+(2**195)+(2**200)+(2**204)+(2**205)+(2**208)+(2**212)+(2**214)+(2**215)+(2**217)+(2**218)+(2**219)+(2**222)+(2**229)+(2**230)+(2**231)+(2**232)+(2**233)+(2**234)+(2**235)+(2**236)+(2**237)+(2**238)+(2**239)+(2**243)+(2**250)+(2**252)+(2**255);
  
  
  logic [255:0] vecx0  ;
  logic [255:0] vecx1  ;
  logic [255:0] vecx2  ;    
  logic [255:0] vecx3  ;
  logic [255:0] vecx4  ;
  logic [255:0] vecx5  ;
  logic [255:0] vecx6  ;
  logic [255:0] vecx7  ;
  logic [255:0] vecx8  ;
  logic [255:0] vecx9  ; 
  
  assign vecx0 =  256'h1fff0123190c3f4c442914c1fa6242094c0fd3142034c07d98c2034c43f4c421;
  assign vecx1 =  256'he7ff02012299c790c848a98e3c86840e9871e43840298b8f21843290dc790c42;
  assign vecx2 =  256'hf87f44443532d9299088d316c94d084931b64a6084521db253084129ad929884;
  assign vecx3 =  256'hff8f10892a636a532109263b5298108643da94c1086536d4a610815336a53108;
  assign vecx4 =  256'hfff0a81114c7b4a60211c87da5302104a6ed2982124a67694c2102a63b4a6210;
  assign vecx5 =  256'h00913ffe40fe4310931407e5310898e03f531084c681fd3108262c0fd3108531;
  assign vecx6 =  256'h0120cffe871ca621243a38ea621121b1c7a621090e8e3a6210c87871e4310a43;
  assign vecx7 =  256'h2242f0fecb654c420a625b34c4205362d94c42129b16cc862114d4b64a6210a6;
  assign vecx8 =  256'h4804ff1f8daa988414c46d498842a6536a90c425319b514c422994da94c4214c;
  assign vecx9 =  256'h9408ffe04ed131086986769218854c53b429884a631da298845318ed29884298 ;
  
  logic [9:0] tes;
  
  assign tes[0] = (vec0 == vecx0) ;
  assign tes[1] = (vec1 == vecx1) ;
  assign tes[2] = (vec2 == vecx2) ;
  assign tes[3] = (vec3 == vecx3) ;
  assign tes[4] = (vec4 == vecx4) ;
  assign tes[5] = (vec5 == vecx5) ;
  assign tes[6] = (vec6 == vecx6) ;
  assign tes[7] = (vec7 == vecx7) ;
  assign tes[8] = (vec8 == vecx8) ;
  assign tes[9] = (vec9 == vecx9) ;
*/
  
endmodule // fxr_10b_ecc_gen
