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
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : Madison Bay (MBY)
// -- Description  : Mesh column write interface 
// ---------------------------------------------------------------------------------------------------------------------

// This is a connectivity only interface and thus is just a named bundle of nets that can be passed around as a group 
// to save typing.  Anywhere an interface is defined, individual nets in the interface can be referenced as follows:    
//
//      <interface name>.<net name>
//

`include "mby_msh_defines.vh"                                   // include file with `defines 

interface mby_msh_col_wr_if 
import mby_msh_pkg::*;                                         // import declarations from mby_msh_pkg.sv
();

msh_col_wr_req_t    i_wr_req                [NUM_MSH_PLANES-1:0];
msh_col_wr_req_t    o_wr_req                [NUM_MSH_PLANES-1:0];
msh_dbus_t          i_wr_dbus               [NUM_MSH_PLANES-1:0];
msh_dbus_t          o_wr_dbus               [NUM_MSH_PLANES-1:0];
logic               i_crdt_rtn_for_wr_req   [NUM_MSH_PLANES-1:0];
logic               o_crdt_rtn_for_wr_req   [NUM_MSH_PLANES-1:0];

modport requestor (

    // inputs
    
    input i_crdt_rtn_for_wr_req,

    // outputs
    
    output o_wr_req,
    output o_wr_dbus

);

modport responder (

    // inputs
    
    input  i_wr_req,
    input  i_wr_dbus,

    // outputs
    
    output o_crdt_rtn_for_wr_req

);

assign i_wr_req                 = o_wr_req;
assign i_wr_dbus                = o_wr_dbus;
assign i_crdt_rtn_for_wr_req    = o_crdt_rtn_for_wr_req;


endinterface // mby_msh_col_wr_if
