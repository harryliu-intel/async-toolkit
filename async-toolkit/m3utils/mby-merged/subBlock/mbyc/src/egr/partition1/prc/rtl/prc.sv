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
// -- Author       : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : Packet Read Controller
// -- Modified     : 12/04/2018 -- Isaac Perez Andrade
//------------------------------------------------------------------------------

module prc 

import shared_pkg::*;
import mby_gmm_pkg::*;
import egr_int_pkg::*;

import shared_pkg::EPL_PER_MGP;
//import egr_int_pkg::wd_rreq_t;
//import egr_int_pkg::req_id_t;
//import egr_int_pkg::client_id_t;
//import egr_int_pkg::word_type_t;


(
    input logic             clk,
    input logic           rst_n, 

    //EGR Internal Interfaces    
    egr_dp_if.requestor             dpb_if, //Dirty Pointer Interface. Requests from the Dirty Pointer Broker
    egr_prc_lcm_if.prc              lcm_if, //Packet Read Controller - Local Congestion Manager Interface
    egr_prc_tmu_if.prc              tmu_if, //Packet Read Controller - Tag Management Unit      Interface
    egr_pfs_prc_if.prc              pfs_if, //Packet Fetch Scheduler - Packet Read Controller   Interface
    egr_rrq_if.requestor           mri_if0, //Read Request Interface.  Gives service to EPL 0,1
    egr_rrq_if.requestor           mri_if1, //Read Request Interface.  Gives service to EPL 2,3
    egr_rrs_id_if.requestor rrs_id_mri_if0, //Read Response ID Interface 0. Gives service to EPL0,1
    egr_rrs_id_if.requestor rrs_id_mri_if1, //Read Response ID Interface 1. Gives service to EPL2,3
    egr_prc_tqu_if.prc              tqu_if  //Transmit Queuing Unit  - Packet Read Controller   Interface

);

///////////////////////////////////////////////////////////////////////////////
// JMG: TEMP CODE FOR FIRST_PACKET
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

always_comb begin
    tmu_if.qsel = {(16*17*16){1'b0}};
    tmu_if.qsel[0][0][0] = pfs_if.valid[0] && pfs_if.ready[0];
end

// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
// JMG: END TEMP CODE FOR FIRST_PACKET
///////////////////////////////////////////////////////////////////////////////

// Local parameters
localparam PORTS_PER_EPL             = 4;
localparam TX_TC_COUNT               = 8;
localparam DATA_TRANSMIT_QUEUE_COUNT = 8;
localparam N_RREQS                   = 1;

// State definition
typedef enum {WAIT_FETCH_REQ, GET_TAG, WORD_FETCH_DATA, WORD_FETCH_CTRL} state_t;
state_t state_r, state;

//typedef enum logic { WORD_TYPE_CTRL, WORD_TYPE_DATA} word_type_t;

// Tag 
mby_tag_ring_t [1:0]   tag, tag_r;

// Internal signals
logic [MBY_TAG_LENGTH_MSB:0] word_cntr, word_cntr_r;

logic [EPL_PER_MGP-1:0][$clog2(PORTS_PER_EPL)-1:0]            port, port_r; // Indicates the port of the winning packet for the EPL
logic [EPL_PER_MGP-1:0][$clog2(MGP_COUNT)-1:0]                 mgp, mgp_r; // Indicates the source MGP of the winning queue
logic [EPL_PER_MGP-1:0][$clog2(TX_TC_COUNT)-1:0]                tc,  tc_r; // Indicates the TC of the winning queue
logic [EPL_PER_MGP-1:0][$clog2(DATA_TRANSMIT_QUEUE_COUNT)-1:0] dtq, dtq_r; // Indicates the data transmit queue of the winning queue

// PRC to MRI
wd_rreq_t [N_RREQS-1:0]       wd_rreq,       wd_rreq_r; // Word Read Request 
logic     [N_RREQS-1:0] wd_rreq_valid, wd_rreq_valid_r; // Word Read Request Valid
logic     [N_RREQS-1:0]   wd_rreq_ack,   wd_rreq_ack_r; // Word Read Request Acknowledge

req_id_t  req_id, req_id_r;

// Register process
always_ff @ (posedge clk, negedge rst_n) begin : state_reg
    if(!rst_n)begin
        state_r     <= WAIT_FETCH_REQ;

        // From PFS
        port_r <= '0;
        mgp_r  <= '0;
        tc_r   <= '0;
        dtq_r  <= '0;

        // From TMU
        tag_r  <= '0;

        req_id_r.spare <= '0;
        req_id_r.data_wd_id <= '0;

        word_cntr_r <= 0;
    end
    else begin
        state_r     <= state;

        // From PFS
        port_r <= port;
        mgp_r  <= mgp;
        tc_r   <= tc;
        dtq_r  <= dtq;

        // From TMU
        tag_r  <= tag;
    
        req_id_r <= req_id;

        word_cntr_r <= word_cntr;
    end
end

// Indicate PFS that a packet is accepted
logic [7:0] upper_cnt_limit;

// Signals for ID
logic            sop; // Start of packet                                      - 1 bit
logic            eop; // End of packet                                        - 1 bit
logic             md; // TODO: ?? Metadata                                             - 2 bits
logic [1:0] port_sel; // Port select  - 2 bits
logic [3:0]   tc_sel; // TX TC select - 3 bits
logic [8:0]       id;

// Next state logic
always_comb begin : next_state

    // Keep previous values from PFS
    port = port_r;
    mgp  = mgp_r;
    tc   = tc_r;
    dtq  = dtq_r;
    // Send ready signal to PFS
    pfs_if.ready = state_r != WAIT_FETCH_REQ; //TODO: Change this to support all TOQs 

    // Tag
    tag = tag_r;
    mri_if0.wd_rreq_valid = state_r == WORD_FETCH_DATA || state_r == WORD_FETCH_CTRL;

    // Create id
    sop      = tmu_if.tag[0].eop; // FIXME: For first packet in isolation SOP and EOP arrive in the same clock cycle
    eop      = tmu_if.tag[0].eop; // 
    md       = 0;                    // TODO:
    port_sel = port_r;
    tc_sel   = tc_r;
    id       = {sop,eop,md,port_sel,tc_sel};


    for(int i=0; i<2; i++)begin

            mri_if0.wd_rreq[i].seg_handle.seg_ptr            = '0;

            mri_if0.wd_rreq[i].seg_handle.sema               = '0;
            mri_if0.wd_rreq[i].wd_sel                        = '0;
            mri_if0.wd_rreq[i].req_id.client_id              = CLIENT_PRC_TQU0_0;

            mri_if0.wd_rreq[i].req_id.data_wd_id.word_type   = WORD_TYPE_DATA;
            mri_if0.wd_rreq[i].req_id.data_wd_id.epl         = '0;
            mri_if0.wd_rreq[i].req_id.data_wd_id.tx_lp       = '0;
            mri_if0.wd_rreq[i].req_id.data_wd_id.tx_tc       = '0;

            mri_if0.wd_rreq[i].service_class                 = '0;
            mri_if0.wd_rreq[i].mc                            = '0;
    end
    
    case(state_r)

        // Wait for a fetch request
        WAIT_FETCH_REQ: begin

            if(pfs_if.valid[0])begin // TODO: Only one TOQ supported in this version
                // Read signals coming from PFS
                // These signals are asserted in the same clock cycle as valid
                port = pfs_if.port;
                mgp  = pfs_if.mgp;
                tc   = pfs_if.tc;
                dtq  = pfs_if.dtq;
                
                state = GET_TAG;
            end
            else begin
                state = WAIT_FETCH_REQ;
            end
            word_cntr = 0;

        end

        // Get tag
        GET_TAG: begin
            if(tmu_if.tag[0].valid)begin // TODO: Only one TOQ is considered at this point
                // Get tag from TMU
                tag = tmu_if.tag ;

                state = WORD_FETCH_DATA;
            end
            else begin
                state = GET_TAG;
            end
            word_cntr = 0;
        end

        // Fetch words
        WORD_FETCH_DATA: begin
            state     = WORD_FETCH_CTRL;

            mri_if0.wd_rreq[0].seg_handle.seg_ptr            = tmu_if.tag[0].ptr;

            mri_if0.wd_rreq[0].seg_handle.sema               = '1;
            mri_if0.wd_rreq[0].wd_sel                        = 0;
            mri_if0.wd_rreq[0].req_id.client_id              = CLIENT_PRC_TQU0_0;

            mri_if0.wd_rreq[0].req_id.data_wd_id.word_type   = WORD_TYPE_DATA;
            mri_if0.wd_rreq[0].req_id.data_wd_id.epl         = '0;
            mri_if0.wd_rreq[0].req_id.data_wd_id.tx_lp       = '0;
            mri_if0.wd_rreq[0].req_id.data_wd_id.tx_tc       = '0;

            mri_if0.wd_rreq[0].service_class                 = '0;
            mri_if0.wd_rreq[0].mc                            = '0;
            
            mri_if0.wd_rreq_valid                         = 1'b1;
        end
        // Fetch words
        WORD_FETCH_CTRL: begin
            state     = WAIT_FETCH_REQ;

            mri_if0.wd_rreq[0].seg_handle.seg_ptr            = tmu_if.tag[0].ptr;

            mri_if0.wd_rreq[0].seg_handle.sema               = '1;
            mri_if0.wd_rreq[0].wd_sel                        = 1;
            mri_if0.wd_rreq[0].req_id.client_id              = CLIENT_PRC_TQU0_0;

            mri_if0.wd_rreq[0].req_id.data_wd_id.word_type   = WORD_TYPE_CTRL;
            mri_if0.wd_rreq[0].req_id.data_wd_id.epl         = '0;
            mri_if0.wd_rreq[0].req_id.data_wd_id.tx_lp       = '0;
            mri_if0.wd_rreq[0].req_id.data_wd_id.tx_tc       = '0;

            mri_if0.wd_rreq[0].service_class                 = '0;
            mri_if0.wd_rreq[0].mc                            = '0;

            mri_if0.wd_rreq_valid                         = 1'b1;
        end

        // Default state
        default:state = WAIT_FETCH_REQ;
    endcase
end

//
endmodule : prc
