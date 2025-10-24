// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  package to encapsulate all FC functional sequences.
// -----------------------------------------------------------------------------

package fc_seq_pkg;
  
    import uvm_pkg::*;
    `include "uvm_macros.svh"

    import sla_pkg::*;
    `include "sla_macros.svh"
    `include "sla_defines.svh"
    `include "slu_macros.svh"

    import fc_env_pkg::*;

    import svt_uvm_pkg::*;
    import svt_bfm_shell_uvm_pkg::*;
    import svt_amba_uvm_pkg::*;

    // base sequence where all FC func sequences extends from
    `include "fc_base_seq.svh"

    `ifdef FC_VCD_REPLAY
     import sigaccess_pkg::*;
    `define comment(msg) `uvm_info({get_name()," comment"},msg, UVM_NONE)
    `include "text_base_file_parser.svh"
    `include "csv_file_parser.svh"
    `include "vcd_file_parser.svh"
    `include "vcd_player_seq.svh"
    `endif

    // saola phase sequences
    `include "fc_powergood_seq.svh"
    `include "fc_training_seq.svh"
    `include "fc_config_seq.svh"
    `include "fc_flush_seq.svh"

endpackage






