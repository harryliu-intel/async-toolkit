// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
// ----------------------------------------------------------------------------
// -- Author : Alberto (Beto) Del Rio Ruiz (alberto.del.rio.ruiz@intel.com)
// -- Project Name : Madison Bay (MBY)
// -- Description  : MSH write interface
// ============================================================================
`ifndef MSH_WR_IF_SV
`define MSH_WR_IF_SV

interface msh_wr_if ();
    import mby_msh_pkg::*;

    mshpt_wreq_t     msh_wreq;

    msh_data_t       msh_wr_data;
           
    logic            msh_wr_lat_sat;
    logic            msh_crdt_rtn_for_wreq;
    logic            msh_mcast_crdt_rtn_for_wreq;
  

    modport request(
        output msh_wreq,

        output msh_wr_data,
           
        input  msh_wr_lat_sat,
        input  msh_crdt_rtn_for_wreq,
        input  msh_mcast_crdt_rtn_for_wreq
    );

    modport receive(
        input  msh_wreq,

        input  msh_wr_data,
           
        output msh_wr_lat_sat,
        output msh_crdt_rtn_for_wreq,
        output msh_mcast_crdt_rtn_for_wreq
    );

endinterface : msh_wr_if
`endif  // MSH_WR_IF_SV

