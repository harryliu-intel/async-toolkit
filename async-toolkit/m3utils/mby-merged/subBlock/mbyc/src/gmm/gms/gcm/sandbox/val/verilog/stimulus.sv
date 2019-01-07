///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
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
// -- Project Name : ??? 
// -- Description  : A stimulus object generates a new input request everytime it is randomized.
// ---------------------------------------------------------------------------------------------------------------------

`ifndef STIMULUS_SV
`define STIMULUS_SV

`include "configuration.sv"

class stimulus;

    configuration                   cfg;        // configuration object
    gcm_sim_pkg::mby_mgp_id_t       i_eq_mgp_port;      // input mgp port number
    gcm_sim_pkg::mby_trk_id_t       i_eq_trk_port;      // input track port number
    gcm_sim_pkg::mby_mgp_id_t       i_dq_mgp_port;      // input mgp port number
    gcm_sim_pkg::mby_trk_id_t       i_dq_trk_port;      // input track port number


//    rand tmpl_pkg::req_in_t req;        // the randomized input requests
    rand shared_pkg::mby_gcm_ring_t         req_eq;        // the randomized input requests
    rand mby_gmm_pkg::mby_unicast_deque_t   req_dq;        // the randomized input requests
 
    function new(configuration cfg, gcm_sim_pkg::mby_mgp_id_t i_eq_mgp_port, gcm_sim_pkg::mby_trk_id_t i_eq_trk_port, gcm_sim_pkg::mby_mgp_id_t i_dq_mgp_port, gcm_sim_pkg::mby_trk_id_t i_dq_trk_port);
        this.cfg    = cfg;
        this.i_eq_mgp_port  = i_eq_mgp_port;
        this.i_eq_trk_port  = i_eq_trk_port;
        this.i_dq_mgp_port  = i_dq_mgp_port;
        this.i_dq_trk_port  = i_dq_trk_port;
    endfunction

    //constraint req_constraint {
    //    // request valid bit is determined by config kno:bs
    //    req.vld dist {
    //        1 := (cfg.knob_inp_bubble_denominator-cfg.inp_bubble_numerators[iport]), 
    //        0 := cfg.inp_bubble_numerators[iport]
    //    };
    //}
    constraint req_eq_constraint {
        // request valid bit is determined by config kno:bs
        req_eq.src_port inside {0};
        req_eq.valid inside {1};
    }
    constraint req_dq_constraint {
        // request valid bit is determined by config kno:bs
        req_dq.src_port_id inside {0};
        req_dq.src_tc inside {0};
        req_dq.valid inside {1};
    }
    
endclass

`endif // STIMULUS_SV
