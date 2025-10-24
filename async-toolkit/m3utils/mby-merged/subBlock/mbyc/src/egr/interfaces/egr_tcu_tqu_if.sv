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
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 

/** INTERFACE: egr_tcu_tqu_if
* TQU to TCU Interface
*
* For connecting the Transmit Queuing Unit to the Transmit Controller Unit
* 
*/
interface egr_tcu_tqu_if ();
    import egr_int_pkg::*;

////////////////// SIGNALS
// Service dtq_ctrl_pull
dtq_ctrl_pull_t  [EPL_PER_MGP-1:0]   dtq_ctrl_pull; // TCU DTQ Control Pull Request (2LP_SELx4TC_SEL+2=10b)
dtq_ready_t      [EPL_PER_MGP-1:0]  dtq_ctrl_ready; // TCU DTQ Control (Pkt Metadata) Ready (4LPx9TC=36b)
ctrl_mdata_t     [EPL_PER_MGP-1:0]      ctrl_mdata; // TQU Control Word Metadata (22b)
data_word_t      [EPL_PER_MGP-1:0]       ctrl_word; // TQU Control Word Data (64B)
logic            [EPL_PER_MGP-1:0] ctrl_word_valid; // TQU Control Word Valid (1b)

//Service dtq_data_pull
dtq_data_pull_t  [EPL_PER_MGP-1:0]   dtq_data_pull; // TQU DTQ Data Pull Request (2LP_SELx4TC_SEL+2=10b)
dtq_ready_t      [EPL_PER_MGP-1:0]  dtq_data_ready; // TCU DTQ Data Ready (4LPx9TC=36b)
pkt_word_mdata_t [EPL_PER_MGP-1:0]  pkt_word_mdata; // TQU Pkt Word Metadata (10b)
data_word_t      [EPL_PER_MGP-1:0]        pkt_word; // TQU Pkt Data Word (64B)
logic            [EPL_PER_MGP-1:0] data_word_valid; // TQU Data Word Valid (1b)


////////////////// MODPORTS
// Requestor of data
modport tcu(
    //Service dtq_ctrl_pull
    output  dtq_ctrl_pull,
    input  dtq_ctrl_ready,
    input      ctrl_mdata,
    input       ctrl_word,
    input ctrl_word_valid,
    //Service dtq_data_pull
    output  dtq_data_pull,
    input  dtq_data_ready,
    input  pkt_word_mdata,
    input        pkt_word,
    input data_word_valid
    );

// Provider of data and control (metadata) words
modport tqu(
    //Service dtq_ctrl_pull
    input    dtq_ctrl_pull,
    output  dtq_ctrl_ready,
    output      ctrl_mdata,
    output       ctrl_word,
    output ctrl_word_valid,
    //Service dtq_data_pull
    input    dtq_data_pull,
    output  dtq_data_ready,
    output  pkt_word_mdata,
    output        pkt_word,
    output data_word_valid
    );

endinterface : egr_tcu_tqu_if

