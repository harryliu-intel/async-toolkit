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

interface pre_ppe_header_if ();
  import mby_igr_pkg::*, shared_pkg::*, mby_egr_pkg::*;

    logic                          sop_ack;

    logic [3:0]                    sop_valid; 
    igr_pkt_id_t                   sop_pkt_id;
    sop_md_t                       sop_md;  
    epl_ts_t                       sop_ts;
    logic                          eop_valid; 
    igr_pkt_id_t                   eop_pkt_id;
    sop_md_t                       eop_md;  
    shim_pb_data_t                 sop_data;    // 3 x 64B + ECC

    seg_ptr_t                      wr_seg_ptr; //[19:0]
    sema_t                         wr_sema;    //[ 3:0]

    
modport src (
             input  sop_ack,
             output sop_valid,
             output sop_pkt_id,
             output sop_md,
             output sop_ts,
             output eop_valid,
             output eop_pkt_id,
             output eop_md,
             output sop_data,
             output wr_seg_ptr,
             output wr_sema
             
    );

modport dest (
             input sop_ack,
             input sop_valid,
             input sop_pkt_id,
             input sop_md,
             input sop_ts,
             input eop_valid,
             input eop_pkt_id,
             input eop_md,
             input sop_data,
             input wr_seg_ptr,
             input wr_sema
    );

endinterface : pre_ppe_header_if

