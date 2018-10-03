// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC functional test package
// -----------------------------------------------------------------------------

package fc_test_pkg;
    // packages/includes
    import uvm_pkg::*;
    `include "uvm_macros.svh"

    
`ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
`endif

import sla_pkg::*;
    `include "sla_defines.svh"
    `include "slu_macros.svh"

    import fc_env_pkg::*;
    import fc_seq_pkg::*;

    // base test class
    `include "fc_test.svh"
    `include "fc_base_test.svh"
    `include "fc_cfg_base_test.svh"

    // tests
    `include "FCTestList.svh"

endpackage
