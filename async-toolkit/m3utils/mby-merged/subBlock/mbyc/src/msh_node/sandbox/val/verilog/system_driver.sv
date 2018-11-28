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
// -- Description  : This file drives DUT reset 
// ---------------------------------------------------------------------------------------------------------------------

`ifndef SYSTEM_DRIVER_SV
`define SYSTEM_DRIVER_SV

class system_driver;

    virtual msh_node_dut_if dut_if;

    string name;

    function new(virtual msh_node_dut_if dut_if);

        this.dut_if = dut_if;

        name = "System Driver";

    endfunction

    // Reset system
    task reset();
        dut_if.mhreset = 1;
        dut_if.msreset = 1;
        dut_if.i_eb_node_col = 0;   // eventually should be set via knob 
        dut_if.i_sb_node_row = 0;   // eventually should be set via knob
        repeat (50) @(posedge dut_if.mclk);
        dut_if.mhreset = 0;
        dut_if.msreset = 0;
        repeat (50) @(posedge dut_if.mclk);
    endtask

endclass

`endif // SYSTEM_DRIVER_SV
