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

    // West Side Write Ports
       logic           i_igr_eb_wreq_valid    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       seg_ptr_t       i_igr_eb_wr_seg_ptr    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       sema_t          i_igr_eb_wr_sema       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       wd_sel_t        i_igr_eb_wr_wd_sel     [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       req_id_t        i_igr_eb_wreq_id       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       msh_data_t      i_igr_eb_wr_data       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
           
   
    // East Side Write Ports
       logic           i_igr_wb_wreq_valid    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       seg_ptr_t       i_igr_wb_wr_seg_ptr    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       sema_t          i_igr_wb_wr_sema       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       wd_sel_t        i_igr_wb_wr_wd_sel     [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       req_id_t        i_igr_wb_wreq_id       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
       msh_data_t      i_igr_wb_wr_data       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input

    
    //////////////////////////////
    // Mesh MGP Read Request Ports      FIXME:  add _rreq to signal names
    //////////////////////////////

    // West Side Read Request Ports 
      logic            i_egr_eb_rreq_valid    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      seg_ptr_t        i_egr_eb_seg_ptr       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      sema_t           i_egr_eb_sema          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      wd_sel_t         i_egr_eb_wd_sel        [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      req_id_t         i_egr_eb_req_id        [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
    
        
    // East Side Read Request Ports 
      logic            i_egr_wb_rreq_valid    [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      seg_ptr_t        i_egr_wb_seg_ptr       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      sema_t           i_egr_wb_sema          [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      wd_sel_t         i_egr_wb_wd_sel        [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
      req_id_t         i_egr_wb_req_id        [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];  // input
    

    ////////////////////////
    // Mesh GMM Write Ports 
    ////////////////////////
   
    // North Side Write Port
       logic           i_sb_wreq_valid;  // input
       seg_ptr_t       i_sb_wr_seg_ptr;  // input
       sema_t          i_sb_wr_sema;     // input
       wd_sel_t        i_sb_wr_wd_sel;   // input
       req_id_t        i_sb_wreq_id;     // input
       msh_data_t      i_sb_wr_data;     // input
           
   
    // South Side Write Port
       logic           i_nb_wreq_valid;  // input
       seg_ptr_t       i_nb_wr_seg_ptr;  // input
       sema_t          i_nb_wr_sema;     // input
       wd_sel_t        i_nb_wr_wd_sel;   // input
       req_id_t        i_nb_wreq_id;     // input
       msh_data_t      i_nb_wr_data;     // input
    

    //////////////////////////////
    // Mesh GMM Read Request Ports
    //////////////////////////////

    // North Side Read Request Port 
      logic            i_sb_rreq_valid;     // input
      seg_ptr_t        i_sb_rreq_seg_ptr;   // input
      sema_t           i_sb_rreq_sema;      // input
      wd_sel_t         i_sb_rreq_wd_sel;    // input
      req_id_t         i_sb_rreq_id;        // input
    
        
    // South Side Read Request Port 
      logic            i_nb_rreq_valid;     // input
      seg_ptr_t        i_nb_rreq_seg_ptr;   // input
      sema_t           i_nb_rreq_sema;      // input
      wd_sel_t         i_nb_rreq_wd_sel;    // input
      req_id_t         i_nb_rreq_id;        // input
   
   
//
// DUT outputs  (direction not specified in this interface)
//

    // West Side Write Ports
     xact_credits_t   o_igr_wb_wreq_credits  [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
   
    // East Side Write Ports
     xact_credits_t   o_igr_eb_wreq_credits  [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output

    
    //////////////////////////////
    // Mesh MGP Read Request Ports      FIXME:  add _rreq to signal names
    //////////////////////////////

    // West Side Read Request Ports 
     xact_credits_t   o_egr_wb_rreq_credits  [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
        
    // East Side Read Request Ports 
     xact_credits_t   o_egr_eb_rreq_credits  [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
   
   
    ///////////////////////////////
    // Mesh MGP Read Response Ports
    ///////////////////////////////
        
    // West Side Read Response Ports
     logic                o_egr_wb_rrsp_valid        [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
     rrsp_dest_block_t    o_egr_wb_rrsp_dest_block   [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
     req_id_t             o_egr_wb_rrsp_req_id       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
     msh_data_t           o_egr_wb_rd_data           [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
    
    // East Side Read Response Ports
     logic                o_egr_eb_rrsp_valid        [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
     rrsp_dest_block_t    o_egr_eb_rrsp_dest_block   [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
     req_id_t             o_egr_eb_rrsp_req_id       [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output
     msh_data_t           o_egr_eb_rd_data           [NUM_MSH_ROWS-1:0][NUM_MSH_ROW_PORTS-1:0];   //  output

    ////////////////////////
    // Mesh GMM Write Ports 
    ////////////////////////
   
    // North Side Write Port
     xact_credits_t   o_nb_wreq_credits;   //  output
   
    // South Side Write Port
     xact_credits_t   o_sb_wreq_credits;   //  output
   
    
    //////////////////////////////
    // Mesh GMM Read Request Ports
    //////////////////////////////

    // North Side Read Request Port 
     xact_credits_t   o_nb_rreq_credits;   //  output
        
    // South Side Read Request Port 
     xact_credits_t   o_sb_rreq_credits;   //  output
        
   
    ///////////////////////////////
    // Mesh GMM Read Response Ports
    ///////////////////////////////
        
    // North Side Read Response Port
    
     logic                o_nb_rrsp_valid;        //  output
     rrsp_dest_block_t    o_nb_rrsp_dest_block;   //  output
     req_id_t             o_nb_rrsp_req_id;       //  output
     msh_data_t           o_nb_rd_data;           //  output
    
    // South Side Read Response Port
    
     logic                o_sb_rrsp_valid;        //  output
     rrsp_dest_block_t    o_sb_rrsp_dest_block;   //  output
     req_id_t             o_sb_rrsp_req_id;       //  output
     msh_data_t           o_sb_rd_data;           //  output


endinterface // msh_dut_if
