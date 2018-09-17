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
// -- Description  : Binds for design template assertions. 
// ---------------------------------------------------------------------------------------------------------------------

`ifdef ASSERT_ON

// instantiate module tmpl_arb_assert with instance name a__tmpl_arb_assert into module tmpl_arb 
//   - note:  using bind allows assertions to be separated from DUT logic 
bind tmpl_arb 
    tmpl_arb_assert a__tmpl_arb_assert (
    .cclk               (cclk),
    .i_reset            (i_reset),
    .outp_arb_gnts_arb  (outp_arb_gnts_arb)
);

`endif // ASSERT_ON

`ifdef COVER_ON

// put coverage binds here

`endif // COVER_ON
