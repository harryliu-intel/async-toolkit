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
// -- Description  : MIG read interface
// ============================================================================
`ifndef MIG_RD_IF_SV
`define MIG_RD_IF_SV

interface mig_rd_if ();
    //
    // FIXME: remove dependency from egr pkg. This should be part of msh pkg
    //        or some pkg for mesh clients
    //
    import mby_egr_pkg::*;

    logic                      mig_rreq_valid;
    logic [W_SEG_PTR-1:0]      mig_seg_ptr;      //[19:0]
    logic [W_SEMA-1:0]         mig_sema;         //[ 3:0]
    logic [W_WD_SEL-1:0]       mig_wd_sel;       //[ 2:0]
    logic [W_REQ_ID-1:0]       mig_req_id;       //[12:0]
    logic [W_XACT_CREDITS-1:0] mig_rreq_credits; // temp value   


    logic                         mig_rrsp_valid;
    logic [W_RRSP_DEST_BLOCK-1:0] mig_rrsp_dest_block;  //[2:0]
    logic [W_REQ_ID-1:0]          mig_rrsp_req_id;      //[12:0]
    logic [W_WORD_BITS-1:0]       mig_rd_data;          //64 x 8

    modport request(
        output mig_rreq_valid,
        output mig_seg_ptr,
        output mig_sema,
        output mig_wd_sel,
        output mig_req_id,
        input  mig_rreq_credits,
        input  mig_rrsp_valid,
        input  mig_rrsp_dest_block,
        input  mig_rrsp_req_id,    
        input  mig_rd_data        
    );  

    modport receive(
        input  mig_rreq_valid,
        input  mig_seg_ptr,
        input  mig_sema,
        input  mig_wd_sel,
        input  mig_req_id,
        output mig_rreq_credits,
        output mig_rrsp_valid,
        output mig_rrsp_dest_block,
        output mig_rrsp_req_id,    
        output mig_rd_data        
    );

endinterface : mig_rd_if
`endif  // MIG_RD_IF_SV

