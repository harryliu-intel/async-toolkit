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
// -- Author : Steve Olson <steve.olson@intel.com>
// -- Project Name : Madison Bay (MBY)
// -- Description  : Mesh memory interface 
// ---------------------------------------------------------------------------------------------------------------------

// This is a connectivity only interface and thus is just a named bundle of nets that can be passed around as a group 
// to save typing.  Anywhere an interface is defined, individual nets in the interface can be referenced as follows:    
//
//      <interface name>.<net name>
//

`include "mby_msh_defines.vh"                                   // include file with `defines 

interface mby_mem_msh_bank_ram_shell_4096x552_func_if
import mby_msh_pkg::*;                                         // import declarations from mby_msh_pkg.sv
();

    /* input  */ logic               wr_en, rd_en;
    /* input  */ logic[11:0]         adr;
    /* input  */ logic[551:0]        wr_data;
    /* output */ logic               rd_valid;
    /* output */ logic[551:0]        rd_data;

    modport mem (
       input  /* logic          */   wr_en, rd_en,
       input  /* logic[11:0]    */   adr,
       input  /* logic[551:0]   */   wr_data,
       output /* logic          */   rd_valid,
       output /* logic[551:0]   */   rd_data
    );

    modport rtl (
       output /* logic          */   wr_en, rd_en,
       output /* logic[11:0]    */   adr,
       output /* logic[551:0]   */   wr_data,
       input  /* logic          */   rd_valid,
       input  /* logic[551:0]   */   rd_data
    );

endinterface // mby_mem_msh_bank_ram_shell_4096x552_func_if
