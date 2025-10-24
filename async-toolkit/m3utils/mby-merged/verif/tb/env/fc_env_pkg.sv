// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC Env package
// -----------------------------------------------------------------------------


package fc_env_pkg;
    import uvm_pkg::*;
    `include "uvm_macros.svh"

    import sla_pkg::*;
    `include "sla_macros.svh"
    `include "sla_defines.svh"
    `include "slu_macros.svh"

    import svt_uvm_pkg::*;
    import svt_bfm_shell_uvm_pkg::*;
    import svt_amba_uvm_pkg::*;

    // FC UVM reg
    import fc_uvm_reg_pkg::*;

    `include "FC.svh"
    `include "fc_im_env.svh"
    `include "fc_report_server.svh"
    `include "fc_mem_access.svh"
   
    // config object
    `include "fc_chip_cfg_obj.svh"
    `include "fc_cfg_obj.svh"

    // fc file name class for managing all the files
    `include "fc_file_names.svh"

    `include "subsystem_base_env.svh"

    // IGR 
    `ifdef IGR_ENV_ENABLE
        import mby_igr_env_pkg::*;
        `include "igr_integ_env.svh"
    `endif // IGR_ENV_ENABLE

    // EPC 
    `ifdef EPC_ENV_ENABLE
        import mby_ec_env_pkg::*;
        `include "epc_integ_env.svh"
    `endif // EPC_ENV_ENABLE

    // PEP
    `ifdef CDN_PCIE_ENV_ENABLE
        import DenaliSvPcie::*;
        import DenaliSvMem::*;
        import cdnPcieUvm::*;
        import cdn_pcie_pkg::*;
        `include "pep_integ_env.svh"
    `endif

    // TB environment
    `include "fc_sm_env.svh"
    `include "fc_tb_env.svh"

endpackage
