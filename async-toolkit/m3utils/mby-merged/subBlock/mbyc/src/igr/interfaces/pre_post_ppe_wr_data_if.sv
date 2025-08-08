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
// -- Description  : 
//                   
//                   
//------------------------------------------------------------------------------

interface pre_post_ppe_wr_data_if ();
  import mby_igr_pkg::*, shared_pkg::*, mby_egr_pkg::*;

    logic                 valid; 
    seg_ptr_t             wr_seg_ptr; //[19:0]
    sema_t                wr_sema;    //[ 3:0]
    wd_sel_t              wd_sel;     //[ 2:0]
    data64_w_ecc_t [0:7]  data_ecc;   // (64 + 8) *8

    
modport src (
                 output valid,
                 output wr_seg_ptr,
                 output wr_sema,
                 output wd_sel,
                 output data_ecc
    );

modport dest (
                 input valid,
                 input wr_seg_ptr,
                 input wr_sema,
                 input wd_sel,
                 input data_ecc
    );

endinterface : pre_post_ppe_wr_data_if

