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
// -- Description  : The Mesh DUT interface 
// ---------------------------------------------------------------------------------------------------------------------

// This is a connectivity only interface and thus is just a named bundle of nets that can be passed around as a group 
// to save typing.  Anywhere an interface is defined, individual nets in the interface can be referenced as follows:    
//
//      <interface name>.<net name>
//

interface msh_node_dut_if
import mby_msh_pkg::*;
import mby_msh_node_pkg::*;
(
   input mclk                                        // mclk is passed in a parameter and becomes part of the interface
);


// local paramters


// DUT inputs  (direction not specified in this interface)
logic               mhreset;                                // reset
logic               msreset;                                // reset

msh_col_t           i_eb_node_col;
msh_row_t           i_sb_node_row;

msh_col_wr_req_t i_nb_wr_req  [NUM_MSH_PLANES-1:0];    // incoming northbound write request 
msh_col_wr_req_t i_sb_wr_req  [NUM_MSH_PLANES-1:0];    // incoming southbound write request 
msh_row_wr_req_t i_eb_wr_req  [NUM_MSH_PLANES-1:0];    // incoming eastbound write request 
msh_row_wr_req_t i_wb_wr_req  [NUM_MSH_PLANES-1:0];    // incoming westbound write request 

msh_dbus_t       i_nb_wr_dbus [NUM_MSH_PLANES-1:0];    // incoming northbound write data bus 
msh_dbus_t       i_sb_wr_dbus [NUM_MSH_PLANES-1:0];    // incoming southbound write data bus 
msh_dbus_t       i_eb_wr_dbus [NUM_MSH_PLANES-1:0];    // incoming eastbound write data bus 
msh_dbus_t       i_wb_wr_dbus [NUM_MSH_PLANES-1:0];    // incoming westbound write data bus 

msh_col_rd_req_t i_nb_rd_req  [NUM_MSH_PLANES-1:0];    // incoming northbound read request 
msh_col_rd_req_t i_sb_rd_req  [NUM_MSH_PLANES-1:0];    // incoming southbound read request 
msh_row_rd_req_t i_eb_rd_req  [NUM_MSH_PLANES-1:0];    // incoming eastbound read request 
msh_row_rd_req_t i_wb_rd_req  [NUM_MSH_PLANES-1:0];    // incoming westbound read request 

msh_col_rd_rsp_t i_nb_rd_rsp  [NUM_MSH_PLANES-1:0];    // incoming northbound read response 
msh_col_rd_rsp_t i_sb_rd_rsp  [NUM_MSH_PLANES-1:0];    // incoming southbound read response 
msh_row_rd_rsp_t i_eb_rd_rsp  [NUM_MSH_PLANES-1:0];    // incoming eastbound read response 
msh_row_rd_rsp_t i_wb_rd_rsp  [NUM_MSH_PLANES-1:0];    // incoming westbound read response 

msh_dbus_t       i_nb_rd_dbus [NUM_MSH_PLANES-1:0];    // incoming northbound read data bus 
msh_dbus_t       i_sb_rd_dbus [NUM_MSH_PLANES-1:0];    // incoming southbound read data bus 
msh_dbus_t       i_eb_rd_dbus [NUM_MSH_PLANES-1:0];    // incoming eastbound read data bus 
msh_dbus_t       i_wb_rd_dbus [NUM_MSH_PLANES-1:0];    // incoming westbound read data bus 

    // credit returns for the corresponding incoming messages
logic            i_sb_crdt_rtn_for_nb_wr_req   [NUM_MSH_PLANES-1:0];
logic            i_nb_crdt_rtn_for_sb_wr_req   [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  i_wb_crdt_rtns_for_eb_wr_reqs [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  i_eb_crdt_rtns_for_wb_wr_reqs [NUM_MSH_PLANES-1:0];

logic            i_sb_crdt_rtn_for_nb_rd_req   [NUM_MSH_PLANES-1:0];
logic            i_nb_crdt_rtn_for_sb_rd_req   [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  i_wb_crdt_rtns_for_eb_rd_reqs [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  i_eb_crdt_rtns_for_wb_rd_reqs [NUM_MSH_PLANES-1:0];

logic            i_sb_crdt_rtn_for_nb_rd_rsp   [NUM_MSH_PLANES-1:0];
logic            i_nb_crdt_rtn_for_sb_rd_rsp   [NUM_MSH_PLANES-1:0];
logic            i_wb_crdt_rtn_for_eb_rd_rsp   [NUM_MSH_PLANES-1:0];
logic            i_eb_crdt_rtn_for_wb_rd_rsp   [NUM_MSH_PLANES-1:0];



// DUT outputs  (direction not specified in this interface)

msh_col_t        o_eb_node_col;                        // the column number for the next node to the east 
msh_row_t        o_sb_node_row;                        // the row number for the next node to the south 

msh_col_wr_req_t o_nb_wr_req  [NUM_MSH_PLANES-1:0];    // outgoing northbound write request 
msh_col_wr_req_t o_sb_wr_req  [NUM_MSH_PLANES-1:0];    // outgoing southbound write request 
msh_row_wr_req_t o_eb_wr_req  [NUM_MSH_PLANES-1:0];    // outgoing eastbound write request 
msh_row_wr_req_t o_wb_wr_req  [NUM_MSH_PLANES-1:0];    // outgoing westbound write request 

msh_dbus_t       o_nb_wr_dbus [NUM_MSH_PLANES-1:0];    // outgoing northbound write data bus 
msh_dbus_t       o_sb_wr_dbus [NUM_MSH_PLANES-1:0];    // outgoing southbound write data bus 
msh_dbus_t       o_eb_wr_dbus [NUM_MSH_PLANES-1:0];    // outgoing eastbound write data bus 
msh_dbus_t       o_wb_wr_dbus [NUM_MSH_PLANES-1:0];    // outgoing westbound write data bus 

msh_col_rd_req_t o_nb_rd_req  [NUM_MSH_PLANES-1:0];    // outgoing northbound read request 
msh_col_rd_req_t o_sb_rd_req  [NUM_MSH_PLANES-1:0];    // outgoing southbound read request 
msh_row_rd_req_t o_eb_rd_req  [NUM_MSH_PLANES-1:0];    // outgoing eastbound read request 
msh_row_rd_req_t o_wb_rd_req  [NUM_MSH_PLANES-1:0];    // outgoing westbound read request 

msh_col_rd_rsp_t o_nb_rd_rsp  [NUM_MSH_PLANES-1:0];    // outgoing northbound read response 
msh_col_rd_rsp_t o_sb_rd_rsp  [NUM_MSH_PLANES-1:0];    // outgoing southbound read response 
msh_row_rd_rsp_t o_eb_rd_rsp  [NUM_MSH_PLANES-1:0];    // outgoing eastbound read response 
msh_row_rd_rsp_t o_wb_rd_rsp  [NUM_MSH_PLANES-1:0];    // outgoing westbound read response 

msh_dbus_t       o_nb_rd_dbus [NUM_MSH_PLANES-1:0];    // outgoing northbound read data bus 
msh_dbus_t       o_sb_rd_dbus [NUM_MSH_PLANES-1:0];    // outgoing southbound read data bus 
msh_dbus_t       o_eb_rd_dbus [NUM_MSH_PLANES-1:0];    // outgoing eastbound read data bus 
msh_dbus_t       o_wb_rd_dbus [NUM_MSH_PLANES-1:0];    // outgoing westbound read data bus 

// credit returns for the corresponding incoming messages
logic            o_sb_crdt_rtn_for_nb_wr_req   [NUM_MSH_PLANES-1:0];
logic            o_nb_crdt_rtn_for_sb_wr_req   [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  o_wb_crdt_rtns_for_eb_wr_reqs [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  o_eb_crdt_rtns_for_wb_wr_reqs [NUM_MSH_PLANES-1:0];

logic            o_sb_crdt_rtn_for_nb_rd_req   [NUM_MSH_PLANES-1:0];
logic            o_nb_crdt_rtn_for_sb_rd_req   [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  o_wb_crdt_rtns_for_eb_rd_reqs [NUM_MSH_PLANES-1:0];
msh_row_crdts_t  o_eb_crdt_rtns_for_wb_rd_reqs [NUM_MSH_PLANES-1:0];

logic            o_sb_crdt_rtn_for_nb_rd_rsp   [NUM_MSH_PLANES-1:0];
logic            o_nb_crdt_rtn_for_sb_rd_rsp   [NUM_MSH_PLANES-1:0];
logic            o_wb_crdt_rtn_for_eb_rd_rsp   [NUM_MSH_PLANES-1:0];
logic            o_eb_crdt_rtn_for_wb_rd_rsp   [NUM_MSH_PLANES-1:0];


endinterface // msh_node_dut_if
