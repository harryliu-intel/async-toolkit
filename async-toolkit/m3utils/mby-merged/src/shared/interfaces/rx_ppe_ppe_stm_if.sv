///------------------------------------------------------------------------------
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
// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Jon Bagge <jon.bagge@intel.com>
// -- Project Name : Madison Bay
// -- Description : RX PPE to shared table memory interface
// --
// -------------------------------------------------------------------

interface rx_ppe_ppe_stm_if
();
logic           [3:0]   tbl0_wen;       //per chunk write enables
logic   [2:0]   [3:0]   tbl0_ren;       //per port, per chunk read enables (port 0 is LPM, 1-2 are EM)
logic   [3:0]   [19:0]  tbl0_addr;      //per port address (port 0 is write, 1 is LPM, 2-3 are EM)
logic           [287:0] tbl0_wdata;     //write data (including ECC bits)
logic           [143:0] tbl0_lpm_rdata; //LPM port read data (including ECC bits)
logic   [1:0]   [287:0] tbl0_em_rdata;  //EM ports read data (including ECC bits)

logic           [3:0]   tbl1_wen;       //per chunk write enables
logic   [1:0]   [3:0]   tbl1_ren;       //per port, per chunk read enables
logic   [2:0]   [19:0]  tbl1_addr;      //per port address (port 0 is write, 1-2 are EM)
logic           [287:0] tbl1_wdata;     //write data (including ECC bits)
logic   [1:0]   [287:0] tbl1_em_rdata;  //EM ports read data (including ECC bits)

modport ppe(
    output  tbl0_wen,
    output  tbl0_ren,
    output  tbl0_addr,
    output  tbl0_wdata,
    input   tbl0_lpm_rdata,
    input   tbl0_em_rdata,
    output  tbl1_wen,
    output  tbl1_ren,
    output  tbl1_addr,
    output  tbl1_wdata,
    input   tbl1_em_rdata
);

modport stm(
    input   tbl0_wen,
    input   tbl0_ren,
    input   tbl0_addr,
    input   tbl0_wdata,
    output  tbl0_lpm_rdata,
    output  tbl0_em_rdata,
    input   tbl1_wen,
    input   tbl1_ren,
    input   tbl1_addr,
    input   tbl1_wdata,
    output  tbl1_em_rdata
);

endinterface: rx_ppe_ppe_stm_if
