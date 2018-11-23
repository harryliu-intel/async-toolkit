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
// -- Description  : This is the interface for connecting 
//                   Egress (EGR) MC and Mirror Pod Ring Stops
//------------------------------------------------------------------------------

interface egr_mc_mirror_pod_if ();
  import mby_gmm_pkg::*;
    
   // pod pointer ring interface (multicast/mirror)
   mby_pod_ptr_ring_t egr_mc_mirror_pod_ring_pod_ptr;
   mby_pod_ptr_ring_t mc_mirror_pod_ring_egr_pod_ptr;
   logic             egr_mc_mirror_pod_ring_stall;
   logic             mc_mirror_pod_ring_egr_stall;

modport egr(
    input  mc_mirror_pod_ring_egr_pod_ptr,
    input    mc_mirror_pod_ring_egr_stall,
    output egr_mc_mirror_pod_ring_pod_ptr,
    output   egr_mc_mirror_pod_ring_stall
    );

modport mc_mirror_pod_ring(
    output mc_mirror_pod_ring_egr_pod_ptr,
    output   mc_mirror_pod_ring_egr_stall,
    input  egr_mc_mirror_pod_ring_pod_ptr,
    input    egr_mc_mirror_pod_ring_stall
    ); 
 
endinterface : egr_mc_mirror_pod_if
