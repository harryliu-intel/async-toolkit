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
// -- Description  : EGR Congestion Management Ring Interface
//                   This is the interface for interconnecting 
//                   Egress (EGR) with the Congestion Management Ring
//------------------------------------------------------------------------------

interface egr_cmring_if ();
  import mby_gmm_pkg::*;

   mby_unicast_deque_t          mby_deque_0;
   mby_unicast_deque_t          mby_deque_1;
   mby_cm_shared_mem_tx_wm_t tx_cm_sm_wm_in;

modport egr(
    input tx_cm_sm_wm_in,
    output   mby_deque_0,
    output   mby_deque_1
    );

modport cmring(
    output tx_cm_sm_wm_in,
    input     mby_deque_0,
    input     mby_deque_1
    );
    
endinterface : egr_cmring_if 
