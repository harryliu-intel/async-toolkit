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
///  ------------------------------------------------------------------------------
///  -- Author       : Isaac Perez-Andrade
///  -- Project Name : Madison Bay (MBY) 
///  -- Description  : Tag Management Unit (TMU) interface with 
///                    Packet Read Controller (PRC) 
///  ------------------------------------------------------------------------------

interface egr_prc_tmu_if();
  import shared_pkg::*;
  import mby_gmm_pkg::*;

    mby_tag_ring_t [1:0] tag;//temp use of tag_ring - bits can be removed
    logic [MGP_COUNT-1:0][MGP_PORT_CNT-1:0][15:0] qsel;//one-hot - [MGP][VP,EGR][TCG] - Best bit order?

// PRC requests from TMU
modport prc(
    input  tag,
    output qsel
    );

// TMU provides to PRC
modport tmu(
    input  qsel,
    output tag
    );

endinterface : egr_prc_tmu_if
