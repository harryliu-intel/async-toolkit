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
//------------------------------------------------------------------------------
// -- Author : Scott Greenfield
// -- Project Name : Madison Bay (MBY) 
// -- Description  : Partial header data from pre_ppe to post_ppe
//                   
//                   
//------------------------------------------------------------------------------

interface pre_post_ppe_partial_data_if ();
  import mby_igr_pkg::*, shared_pkg::*, mby_egr_pkg::*;

    // no need to know which bytes are valid here
    // segment pointer not included here, included in sop metadata

    logic                        valid; 
    igr_pkt_id_t                 pkt_id;
    logic [32*8-1:0]             pdata;       // 32 bytes of partial data


modport src (
                 output valid,
                 output pkt_id,
                 output pdata
    );

modport dest (
                 input valid,
                 input pkt_id,
                 input pdata
    );

endinterface : pre_post_ppe_partial_data_if

