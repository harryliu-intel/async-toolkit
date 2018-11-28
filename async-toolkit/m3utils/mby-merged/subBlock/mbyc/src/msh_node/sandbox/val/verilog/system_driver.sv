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
        $readmemh ("../../tests/mem_bank_0.txt",top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_0.behave_mem.sram);
        $readmemh ("../../tests/mem_bank_1.txt",top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_1.behave_mem.sram);
        $readmemh ("../../tests/mem_bank_2.txt",top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_2.behave_mem.sram);
        $readmemh ("../../tests/mem_bank_3.txt",top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_3.behave_mem.sram);
        // If the MGM BFM model has INTEL_SIMONLY set, the following task call is available
        //  top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_0.memory_init("../../tests/mem_bank_0.txt");
        //  top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_1.memory_init("../../tests/mem_bank_1.txt");
        //  top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_2.memory_init("../../tests/mem_bank_2.txt");
        //  top.msh_node.mby_msh_gen_mem.msh_sram_mems.msh_wrap_mem_msh_bank_ram_shell_4096x552_3.memory_init("../../tests/mem_bank_3.txt");
        repeat (50) @(posedge dut_if.mclk);
        dut_if.mhreset = 0;
        dut_if.msreset = 0;
        repeat (50) @(posedge dut_if.mclk);
        // Initialize mesh memories
    endtask

endclass

`endif // SYSTEM_DRIVER_SV
