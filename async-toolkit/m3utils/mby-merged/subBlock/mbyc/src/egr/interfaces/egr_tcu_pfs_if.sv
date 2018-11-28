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
//------------------------------------------------------------------------------
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : TCU to PFS Interface
//                   For connecting the Transmit Controller Unit to the 
//                   Packet Fetch Scheduler
//------------------------------------------------------------------------------

interface egr_tcu_pfs_if import shared_pkg::*; ();
localparam PORTS_PER_EPL = 4;
localparam RX_TC_COUNT = 16;

// indicates that the corresponding tc is blocked
logic [EPL_PER_MGP-1:0][PORTS_PER_EPL-1:0][RX_TC_COUNT-1:0] pfc;

modport tcu(
    output pfc
    );

modport pfs(
    input pfc
    );

endinterface : egr_tcu_pfs_if

