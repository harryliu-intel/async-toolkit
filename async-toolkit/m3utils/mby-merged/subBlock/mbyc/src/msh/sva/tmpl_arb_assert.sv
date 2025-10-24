///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
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
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : ??? 
// -- Description  : Design template assertions. 
//      - note that the assertion macros are defined in SVA_LIB (see build logs for location of this library)
// ---------------------------------------------------------------------------------------------------------------------

module tmpl_arb_assert (
    input                       cclk,
    input                       i_reset,
    input tmpl_pkg::inp_t       outp_arb_gnts_arb [tmpl_pkg::NUM_OUTPUTS-1:0]
);

generate
    for (genvar gv_o=0; gv_o < tmpl_pkg::NUM_OUTPUTS; gv_o++) begin : asserts
        // I'm not sure why this assertion generates the following warning: 
        //      Warning-[ETRFSFAC] Explicit typecast required for streams for assertion context
        // The assertion seems to work.
        `ASSERTS_AT_MOST_BITS_HIGH(a__arb_gnts, (outp_arb_gnts_arb[gv_o]), 1, cclk, i_reset, `ERR_MSG("outp_arb_gnts_arb has more than 1 hot bit"));
    end : asserts
endgenerate

endmodule : tmpl_arb_assert
