///
///  INTEL CONFIDENTIAL
///
///  Copyright 2018 Intel Corporation All Rights Reserved.
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
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : EGR Tag Ring Interface
//                   This is the interface for interconnecting 
//                   Egress (EGR) with the Tag Ring
//------------------------------------------------------------------------------
/* Up to date in sync with mbay_ring_wires_v02.xlsx last checked on 12-Nov-2018 */

interface egr_tagring_if ();
  import mby_gmm_pkg::*;
//TODO Cleanup of signals

//    lltformat_t             lltformat;
//    lltformat_valid_t lltformat_valid;
    mby_tag_ring_t mby_tag_ring;

modport egr(
//    input         lltformat,
//    input   lltformat_valid 
    input mby_tag_ring
    );
    
modport tagring(
//    output         lltformat,
//    output   lltformat_valid 
    output mby_tag_ring
    );    
    
    
endinterface : egr_tagring_if
