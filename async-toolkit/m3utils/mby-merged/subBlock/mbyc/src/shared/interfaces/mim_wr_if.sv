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
// -- Description  : MIM write interface
//

// =====================================================================================================================

// FIXME:  use msh_wr_if and get rid of this file 

interface mim_wr_if ();
    // old interface
    import mby_egr_pkg::*;
    // new interface
    import mby_msh_pkg::*;

    // old interface
  logic                   mim_wreq_valid; 
  logic [W_SEG_PTR-1:0]   mim_wr_seg_ptr; //[19:0]
  logic [W_SEMA-1:0]      mim_wr_sema;    //[ 3:0]
  logic [W_WD_SEL-1:0]    mim_wr_wd_sel;  //[ 2:0]
  logic [W_REQ_ID-1:0]    mim_wreq_id;    //[12:0]
  logic [W_WORD_BITS-1:0] mim_wr_data;    // 64*8
  
  logic [W_XACT_CREDITS-1:0] mim_wreq_credits; // temp value
  
    // new interface
    mshpt_wreq_t     msh_wreq;

    msh_data_t       msh_wr_data;
           
    logic            msh_wr_lat_sat;
    logic            msh_crdt_rtn_for_wreq;
    logic            msh_mcast_crdt_rtn_for_wreq;

    modport request(
    // old interface
    output mim_wreq_valid,
    output mim_wr_seg_ptr,
    output mim_wr_sema,   
    output mim_wr_wd_sel, 
    output mim_wreq_id,
    output mim_wr_data,  
    input  mim_wreq_credits,
    // new interface
        output msh_wreq,

        output msh_wr_data,
           
        input  msh_wr_lat_sat,
        input  msh_crdt_rtn_for_wreq,
        input  msh_mcast_crdt_rtn_for_wreq
    );

    modport receive(
    // old interface
    input  mim_wreq_valid,
    input  mim_wr_seg_ptr,
    input  mim_wr_sema,   
    input  mim_wr_wd_sel, 
    input  mim_wreq_id,   
    input  mim_wr_data,  
    output mim_wreq_credits,       
    // new interface
        input  msh_wreq,

        input  msh_wr_data,
           
        output msh_wr_lat_sat,
        output msh_crdt_rtn_for_wreq,
        output msh_mcast_crdt_rtn_for_wreq
    );

endinterface : mim_wr_if
