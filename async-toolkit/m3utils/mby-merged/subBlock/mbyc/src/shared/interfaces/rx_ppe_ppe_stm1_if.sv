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
// -- Description : RX PPE to shared table memory 1 interface
// --
// -------------------------------------------------------------------

interface rx_ppe_ppe_stm1_if
();
logic           [3:0]   tbl_wen;        //per chunk write enables
logic   [1:0]   [3:0]   tbl_ren;        //per port, per chunk read enables
logic   [2:0]   [16:0]  tbl_addr;       //per port address (port 0 is write, 1-2 are EM)
logic           [287:0] tbl_wdata;      //write data (including ECC bits)
logic   [1:0]   [287:0] tbl_em_rdata;   //EM ports read data (including ECC bits)

modport ppe(
    output  tbl_wen,
    output  tbl_ren,
    output  tbl_addr,
    output  tbl_wdata,
    input   tbl_em_rdata
);

modport stm(
    input   tbl_wen,
    input   tbl_ren,
    input   tbl_addr,
    input   tbl_wdata,
    output  tbl_em_rdata
);

endinterface: rx_ppe_ppe_stm1_if
