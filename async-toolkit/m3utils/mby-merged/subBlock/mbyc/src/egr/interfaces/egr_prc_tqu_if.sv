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
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : PRC to TQU Interface
//                   For connecting the Packet Read Controller to
//                   the Transmit Queuing Unit
//------------------------------------------------------------------------------

interface egr_prc_tqu_if ();
    import shared_pkg::*;
    
    localparam N_MAX_LP     = 4;
    localparam W_DTQ_SEL    = $clog2(MGP_TC_CNT*N_MAX_LP);
    localparam N_EPL        = 4;

    typedef logic [W_DTQ_SEL-1:0] dtq_sel_t; 

    logic     [N_EPL-1:0]   ctl_valid;
    dtq_sel_t [N_EPL-1:0] dtq_sel_ctl;
    
    logic     [N_EPL-1:0]   data_valid;
    dtq_sel_t [N_EPL-1:0] dtq_sel_data;

modport tqu(
    output    ctl_valid,
    output  dtq_sel_ctl,

    output   data_valid,
    output dtq_sel_data
    );

modport prc(
    input    ctl_valid,
    input  dtq_sel_ctl,

    input   data_valid,
    input dtq_sel_data
    );

endinterface : egr_prc_tqu_if

