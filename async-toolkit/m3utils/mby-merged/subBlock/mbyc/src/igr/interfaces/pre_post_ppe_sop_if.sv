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
// -- Description  : Interface between pre_ppe and post_ppe for data related to 
//                   packet header and tracking of packets in flight in RX PPE.
//                   
//------------------------------------------------------------------------------

interface pre_post_ppe_sop_if ();
  import mby_igr_pkg::*, shared_pkg::*, mby_egr_pkg::*;

    logic                        valid; 
    igr_pkt_id_t                 pkt_id;
    sop_md_t                     md;
    logic [2:0]                  sll;
    seg_ptr_t                    wr_seg_ptr; //[19:0]
    sema_t                       wr_sema;    //[ 3:0]
    
modport src (
                 output valid,
                 output pkt_id,
                 output md,
                 output sll,
                 output wr_seg_ptr,
                 output wr_sema
    );

modport dest (
                 input valid,
                 input pkt_id,
                 input md,
                 input sll,
                 input wr_seg_ptr,
                 input wr_sema
    );

endinterface : pre_post_ppe_sop_if

