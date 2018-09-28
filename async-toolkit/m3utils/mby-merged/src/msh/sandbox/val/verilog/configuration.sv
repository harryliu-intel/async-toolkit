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
// -- Description  : A configuration object generates a randomized configuration based in configuration knob settings.
// ---------------------------------------------------------------------------------------------------------------------

`ifndef CONFIGURATION_SV
`define CONFIGURATION_SV

class configuration;

    // other variables
    string name;

    function new(
    );

        name = "configuration.sv";
    endfunction

    // constrain randomized configuration parameters
    constraint cfg_constraint {
        foreach (inp_bubble_numerators[i])
            inp_bubble_numerators[i] dist {[knob_inp_bubble_numerator_min:knob_inp_bubble_numerator_max] := 1};
    }

    // this function is called by default immediately after randomize()
    function void post_randomize();
        $display("(time: %0d) %s: **Configuration Randomized**", $time, name);
    endfunction

    // print out configuration
    task print();
        $display("");
        $display("---------------------------");
        $display("CONFIGURATION:  input knobs");
        $display("---------------------------");
        $display("knob_inp_bubble_numerator_min = %0d", knob_inp_bubble_numerator_min);
        $display("knob_inp_bubble_numerator_max = %0d", knob_inp_bubble_numerator_max);
        $display("knob_inp_bubble_denominator   = %0d", knob_inp_bubble_denominator);
        $display("");
        $display("------------------------------------");
        $display("CONFIGURATION:  generated parameters");
        $display("------------------------------------");
        foreach (inp_bubble_numerators[i])
            $display("odds of bubble on input port [%0d] = %0d/%0d", i, inp_bubble_numerators[i], knob_inp_bubble_denominator);
        $display("");
    endtask
endclass

`endif // CONFIGURATION_SV
