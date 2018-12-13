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
// -- Description  : MIG write interface
// ============================================================================
`ifndef MIG_WR_IF_SV
`define MIG_WR_IF_SV

interface mig_wr_if ();
    //
    // FIXME: remove dependency from egr pkg. This should be part of msh pkg
    //        or some pkg for mesh clients
    //
    import mby_egr_pkg::*;

    logic                      mig_wreq_valid;
    logic [W_SEG_PTR-1:0]      mig_wr_seg_ptr; //[19:0]
    logic [W_SEMA-1:0]         mig_wr_sema;    //[ 3:0]
    logic [W_WD_SEL-1:0]       mig_wr_wd_sel;  //[ 2:0]
    logic [W_REQ_ID-1:0]       mig_wreq_id;    //[12:0]
    logic [W_WORD_BITS-1:0]    mig_wr_data;    // 64*8

    logic [W_XACT_CREDITS-1:0] mig_wreq_credits; // temp value

    modport request(
        output mig_wreq_valid,
        output mig_wr_seg_ptr,
        output mig_wr_sema,
        output mig_wr_wd_sel,
        output mig_wreq_id,
        output mig_wr_data,
        input  mig_wreq_credits
    );

    modport receive(
        input  mig_wreq_valid,
        input  mig_wr_seg_ptr,
        input  mig_wr_sema,
        input  mig_wr_wd_sel,
        input  mig_wreq_id,
        input  mig_wr_data,
        output mig_wreq_credits
    );

endinterface : mig_wr_if
`endif  // MIG_WR_IF_SV

