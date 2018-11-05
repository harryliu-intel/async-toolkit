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

    configuration           cfg;        // configuration object
    tmpl_pkg::enc_inp_t     iport;      // input port number

    rand tmpl_pkg::req_in_t req;        // the randomized input requests
 
    function new(configuration cfg, tmpl_pkg::enc_inp_t iport);
        this.cfg    = cfg;
        this.iport  = iport;
    endfunction

    constraint req_constraint {
        // request valid bit is determined by config knobs
        req.vld dist {
            1 := (cfg.knob_inp_bubble_denominator-cfg.inp_bubble_numerators[iport]), 
            0 := cfg.inp_bubble_numerators[iport]
        };
    }
    
endclass

`endif // STIMULUS_SV
