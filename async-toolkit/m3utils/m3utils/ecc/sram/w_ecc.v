// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// -------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright (2012) Intel Corporation All Rights Reserved. 
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
// Material is protected by worldwide copyright and trade secret laws and treaty
// provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or disclosed
// in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
// -------------------------------------------------------------------

module w_ecc #( 
  WDTH = 34,   // data bits (8 <= "WDTH" <= 8178)
  CBTS = 7     // check bits(5 <= "CBTS" <= 10)
)(
input  logic [WDTH-1:0] i_data,       // data input bus 
output logic [WDTH-1:0] o_data,       // data output bus 
output logic [CBTS-1:0] o_chk         // checkbit output bus 

);

`ifdef NEW_ECC
  write_ecc #(
    .WDTH(WDTH),    // data size (8 <= "width" <= 8178)
    .CBTS(CBTS)  // number of checkbits (5 <= "chkbits" <= 10)
  ) u_write_ecc (
    .i_data     ( i_data ),      
    .o_data     ( o_data ),       
    .o_chk      ( o_chk )         
  ); 
`else
  logic            wrecc_err_detect_nc; // ecc no connect
  logic            wrecc_err_multpl_nc; // ecc no connect

  DW_ecc #(
    .width(WDTH),    // data size (8 <= "width" <= 8178)
    .chkbits(CBTS),  // number of checkbits (5 <= "chkbits" <= 10)
    .synd_sel(0)     // controls checkbit correction vs syndrome
                     // emission selection when gen input is not
                     // active (0 => correct check bits
                     // 1 => pass syndrome to chkout)
  ) i_DW_ecc (
    .gen        ( 1'b1 ),         // gen tied low for read
    .correct_n  ( 1'b1 ),   // correct_n from system
    .datain     ( i_data ),        // datain from memory
    .chkin      ( {CBTS{1'b0}} ),         // chkin also from memory
    .err_detect ( wrecc_err_detect_nc ),  // error flag to system
    .err_multpl ( wrecc_err_multpl_nc ),  // severe error flag to system
    .dataout    ( o_data ),        // read data (corrected if allowed)
    .chkout     ( o_chk )          // error syndrome for logging
  ); 
`endif

endmodule
