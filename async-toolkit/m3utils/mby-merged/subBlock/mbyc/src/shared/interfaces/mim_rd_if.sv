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
// -- Author : Scott Hussong 
// -- Project Name : Madison Bay (MBY) 
// -- Description  : MIM read interface
//

// =====================================================================================================================

interface mim_rd_if ();
  import mby_egr_pkg::*;
    

  logic                    mim_rreq_valid;
  logic [W_SEG_PTR-1:0]    mim_seg_ptr;      //[19:0]
  logic [W_SEMA-1:0]       mim_sema;         //[ 3:0]
  logic [W_WD_SEL-1:0]     mim_wd_sel;       //[ 2:0]
  logic [W_REQ_ID-1:0]     mim_req_id;       //[12:0]
  
  logic [W_XACT_CREDITS-1:0] mim_rreq_credits; // temp value   
  
  
  logic                         mim_rrsp_valid;
  logic [W_RRSP_DEST_BLOCK-1:0] mim_rrsp_dest_block;  //[2:0]
  logic [W_REQ_ID-1:0]          mim_rrsp_req_id;      //[12:0]
  logic [W_WORD_BITS-1:0]       mim_rd_data;          //64 x 8

    
modport request(
    input  mim_rreq_valid,
    input  mim_seg_ptr,
    input  mim_sema,
    input  mim_wd_sel,
    input  mim_req_id,
    output mim_rreq_credits,
    output mim_rrsp_valid,
    output mim_rrsp_dest_block,
    output mim_rrsp_req_id,    
    output mim_rd_data        
    );

modport receive(
    output mim_rreq_valid,
    output mim_seg_ptr,
    output mim_sema,
    output mim_wd_sel,
    output mim_req_id,
    input  mim_rreq_credits,
    input  mim_rrsp_valid,
    input  mim_rrsp_dest_block,
    input  mim_rrsp_req_id,    
    input  mim_rd_data        
    );

endinterface : mim_rd_if
