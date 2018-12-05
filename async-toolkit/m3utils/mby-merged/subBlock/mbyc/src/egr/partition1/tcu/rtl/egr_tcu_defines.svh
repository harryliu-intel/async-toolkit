//
//  Copyright 2018 - 2028 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
// -- Author       : John Lo          
// -- Project Name : Madison Bay (MBY)
// -- Description  : 
//
// EPL uses a:d this corresponds to 0:3 in ERG.
//------------------------------------------------------------------------------

`define  TOT_EPL                 4
`define  TOT_LP                  4
`define  TOT_PG                  9
`define  TOT_TC                  9
`define  TC_WIDTH                4
`define  QM_WIDTH               20    
`define  DC_WIDTH               `QM_WIDTH + 4   // 64 bytes unit i.e. per clock
`define  PG_WIDTH                4
//`define  CFG_PG_TCG_REG_WIDTH  `TOT_PG+1
`define  SP_WIDTH            4    
`define  SP_TOP0          `SP_WIDTH'h0
`define  SP_TOP1          `SP_WIDTH'h1
`define  SP_TOP2          `SP_WIDTH'h2
`define  SP_TOP3          `SP_WIDTH'h3
`define  SP_TOP4          `SP_WIDTH'h4
`define  SP_TOP5          `SP_WIDTH'h5
`define  SP_TOP6          `SP_WIDTH'h6
`define  SP_TOP7          `SP_WIDTH'h7
`define  SP_TOP8          `SP_WIDTH'h8
//
