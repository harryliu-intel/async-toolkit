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
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : TQU Read Response Receiver
//------------------------------------------------------------------------------

module tqu_rrsp_rcv
    import egr_int_pkg::*;
(
    input logic             clk,
    input logic           rst_n, 

    egr_rrs_if.requestor mri_if, // Read Response Interface. Gives service to 2 EPLs

    output logic       wd_valid [N_EPL_PER_EPP],
    output data_word_t     word [N_EPL_PER_EPP],
    output dtq_sel_t    dtq_sel [N_EPL_PER_EPP],
    output logic      word_type [N_EPL_PER_EPP]  // Word type: 0:Metadata(Control) 1:Data
);


//modport requestor(
//    input     rrsp_wd_id,
//    input        rrsp_wd,
//    input  rrsp_wd_valid,
//    output rrsp_wd_stall
//    );



endmodule : tqu_rrsp_rcv
