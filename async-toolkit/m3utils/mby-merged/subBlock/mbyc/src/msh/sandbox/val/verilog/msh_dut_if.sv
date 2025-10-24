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

interface msh_dut_if
import mby_msh_pkg::*;
import mby_egr_pkg::*;
import shared_pkg::*; 
#(

    parameter int NUM_MSH_ROWS    = MAX_NUM_MSH_ROWS,               // number of mesh rows
    parameter int NUM_MSH_COLS    = MAX_NUM_MSH_COLS                // number of mesh columns


)
(
   input mclk                                        // mclk is passed in a parameter and becomes part of the interface
);


// local paramters


// DUT inputs  (direction not specified in this interface)
logic               chreset;                                // core hard reset
logic               csreset;                                // core soft reset
logic               mhreset;                                // mesh hard reset
logic               msreset;                                // mesh soft reset

    ////////////////////////
    // Mesh MGP Write Ports 
    ////////////////////////
   
    // West Side Write Ports
    
    mshpt_wreq_t     i_igr_eb_wreq                       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    msh_data_t       i_igr_eb_wr_data                    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
           
    logic            o_igr_eb_wr_lat_sat                 [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_igr_crdt_rtn_for_eb_wreq          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_igr_mcast_crdt_rtn_for_eb_wreq    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    
    // East Side Write Ports
    
    mshpt_wreq_t     i_igr_wb_wreq                       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    msh_data_t       i_igr_wb_wr_data                    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
           
    logic            o_igr_wb_wr_lat_sat                 [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_igr_crdt_rtn_for_wb_wreq          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_igr_mcast_crdt_rtn_for_wb_wreq    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
   
    
    //////////////////////////////
    // Mesh MGP Read Request Ports
    //////////////////////////////

    // West Side Read Request Ports 
   
    mshpt_rreq_t     i_egr_eb_rreq                       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    logic            o_egr_eb_rd_lat_sat                 [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_egr_crdt_rtn_for_eb_rreq          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_egr_mcast_crdt_rtn_for_eb_rreq    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
        
    // East Side Read Request Ports 
    
    mshpt_rreq_t     i_egr_wb_rreq                       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    logic            o_egr_wb_rd_lat_sat                 [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_egr_crdt_rtn_for_wb_rreq          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            o_egr_mcast_crdt_rtn_for_wb_rreq    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
        
   
   
    ///////////////////////////////
    // Mesh MGP Read Response Ports
    ///////////////////////////////
        
    // West Side Read Response Ports
    
    logic            i_egr_crdt_rtn_for_wb_rrsp          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            i_egr_mcast_crdt_rtn_for_wb_rrsp    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    mshpt_rrsp_t     o_egr_wb_rrsp                       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    msh_data_t       o_egr_wb_rd_data                    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    
    // East Side Read Response Ports
    
    logic            i_egr_crdt_rtn_for_eb_rrsp          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
    logic            i_egr_mcast_crdt_rtn_for_eb_rrsp    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    mshpt_rrsp_t     o_egr_eb_rrsp                       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];

    msh_data_t       o_egr_eb_rd_data                    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];
   



    ////////////////////////
    // Mesh GMM Write Ports 
    ////////////////////////
   
    // North Side Write Ports
                                                                                  
    mshpt_wreq_t     i_gmn_sb_wreq                                         [NUM_GMN_PORTS-1:0];

    msh_data_t       i_gmn_sb_wr_data                                      [NUM_GMN_PORTS-1:0];
           
    logic            o_gmn_sb_wr_lat_sat                                   [NUM_GMN_PORTS-1:0];
    logic            o_gmn_crdt_rtn_for_sb_wreq                            [NUM_GMN_PORTS-1:0];
    logic            o_gmn_mcast_crdt_rtn_for_sb_wreq                      [NUM_GMN_PORTS-1:0];
   
    // South Side Write Ports
                                                                                  
    mshpt_wreq_t     i_gms_nb_wreq                                         [NUM_GMS_PORTS-1:0];

    msh_data_t       i_gms_nb_wr_data                                      [NUM_GMS_PORTS-1:0];
           
    logic            o_gms_nb_wr_lat_sat                                   [NUM_GMS_PORTS-1:0];
    logic            o_gms_crdt_rtn_for_nb_wreq                            [NUM_GMS_PORTS-1:0];
    logic            o_gms_mcast_crdt_rtn_for_nb_wreq                      [NUM_GMS_PORTS-1:0];
   
   
    
    //////////////////////////////
    // Mesh GMM Read Request Ports
    //////////////////////////////

    // North Side Read Request Ports 
    
    mshpt_rreq_t     i_gmn_sb_rreq                                         [NUM_GMN_PORTS-1:0];

    logic            o_gmn_sb_rd_lat_sat                                   [NUM_GMN_PORTS-1:0];
    logic            o_gmn_crdt_rtn_for_sb_rreq                            [NUM_GMN_PORTS-1:0];
    logic            o_gmn_mcast_crdt_rtn_for_sb_rreq                      [NUM_GMN_PORTS-1:0];
        
    // South Side Read Request Ports 
    
    mshpt_rreq_t     i_gms_nb_rreq                                         [NUM_GMS_PORTS-1:0];

    logic            o_gms_nb_rd_lat_sat                                   [NUM_GMS_PORTS-1:0];
    logic            o_gms_crdt_rtn_for_nb_rreq                            [NUM_GMS_PORTS-1:0];
    logic            o_gms_mcast_crdt_rtn_for_nb_rreq                      [NUM_GMS_PORTS-1:0];
        
   
   
    ///////////////////////////////
    // Mesh GMM Read Response Ports
    ///////////////////////////////
        
    // North Side Read Response Ports
    
    logic            i_gmn_crdt_rtn_for_nb_rrsp                            [NUM_GMN_PORTS-1:0];
    logic            i_gmn_mcast_crdt_rtn_for_nb_rrsp                      [NUM_GMN_PORTS-1:0];

    mshpt_rrsp_t     o_gmn_nb_rrsp                                         [NUM_GMN_PORTS-1:0];

    msh_data_t       o_gmn_nb_rd_data                                      [NUM_GMN_PORTS-1:0];
    
    // South Side Read Response Ports
    
    logic            i_gms_crdt_rtn_for_sb_rrsp                            [NUM_GMS_PORTS-1:0];
    logic            i_gms_mcast_crdt_rtn_for_sb_rrsp                      [NUM_GMS_PORTS-1:0];

    mshpt_rrsp_t     o_gms_sb_rrsp                                         [NUM_GMS_PORTS-1:0];

    msh_data_t       o_gms_sb_rd_data                                      [NUM_GMS_PORTS-1:0];

endinterface // msh_dut_if
