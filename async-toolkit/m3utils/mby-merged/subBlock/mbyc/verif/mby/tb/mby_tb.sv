// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Eric Schmidt (eric.j.schmidt@intel.com)
//   Project       : MBY IP
//------------------------------------------------------------------------------

//  Module:    mby_tb
//
//  MBY IP TB top moudle
//
//   This top module will:
//
//   1. Instance 1 RTL Unit
//
//   2. Control Dumping features for FSDB/DVE.

`timescale 1ps/1ps

`include "mby_defines.sv"
`include "dummy_dut.v"

module mby_tb ();
// parameter   UPF_SIMULATIONS = 1;
// parameter   UPF_SIMULATIONS = 0;


`ifdef XVM
    import ovm_pkg::*;
    import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "slu_macros.svh"
`endif

    import uvm_pkg::*;
// START IOSF_NOT_PRESENT
//    import IosfPkg::*;
// END IOSF_NOT_PRESENT
// START CHASSIS_NOT_PRESENT
//    import CCAgentPkg::*;
//    import PGCBAgentPkg::*;
//    import ccu_vc_pkg::*;
// END CHASSIS_NOT_PRESENT

//   import UPF::*;
  
  
    logic primary_clock;
    logic gated_primary_clock;
    logic secondary_clock;
    logic gated_secondary_clock;

// ===============================================
// =                FSDB variable                =
// ===============================================
    string       fsdb_file = "dump";
    int          fsdb_config_file;
    longint      fsdb_on,fsdb_off;
    int          fsdb_file_size = 1800;
    string       str;
    logic        vccRail;

  
// ===============================================
// Verification Test Library
// ===============================================
    mby_test_lib test();
    
    
// ===============================================
// DUT RTL instance
// ===============================================
   `include "mby_top_inst.v"


// Parameters for interfaces
   `include "mby_params.sv"

// ===============================================
// Interfaces instance
// ===============================================

// START IOSF_NOT_PRESENT
//    // IOSF Primary interface
//    // ===============================================
//    iosf_primary_intf #(`MBY_IOSF_PRI_PARAMS) iosf_pri_if();
//
//    // IOSF SideBand interface
//    // ===============================================
//   `ifdef IOSF_SB_PH2
//    iosf_sbc_intf  iosf_sb_if(.side_clk(secondary_clock),
//                                                    .side_rst_b(mby_if.secondary_reset),
//          .gated_side_clk(gated_secondary_clock),
//          .agent_rst_b(mby_if.secondary_reset));
//   `else
//
//  iosf_sbc_intf #(`MBY_IOSF_SB_PARAMS)  iosf_sb_if(.side_clk(secondary_clock),
//                                                     .side_rst_b(mby_if.secondary_reset),
//           .gated_side_clk(gated_secondary_clock),
//           .agent_rst_b(mby_if.secondary_reset));
//   `endif
// END IOSF_NOT_PRESENT
// START CHASSIS_NOT_PRESENT
//    // Chassis reset pkg interfaces
//    // ===============================================
//    ccu_intf #(`MBY_CCU_VC_PARAMS) ccu_if();
//    PowerGatingIF #(`MBY_CHASSIS_PWRGATE_CCAGENT_PARAMS) pg_if();
//    chassis_misc_if misc_if();
//    chassis_rst_misc_ti #(
//   .IS_ACTIVE(1),
//   .IS_PMC_ENV(0),
//   .CHASSIS_RESET_ENV_UVM_NAME("*mby_chassis_rst_env")
//    )misc_ti_i(misc_if);
// END CHASSIS_NOT_PRESENT

// ===============================================
// Clock block instance
// ===============================================

int idle_conter;
initial begin
    primary_clock = 1'b0;
    gated_primary_clock = 1'b0;
    secondary_clock = 1'b0;
    idle_conter = 0;
end

//INTEG : remove since real RTL should drive this :
initial begin
    force `MBY_TOP_PATH.prim_pok = 0;
// START IOSF_NOT_PRESENT
//    @(posedge iosf_pri_if.powergood_rst_b) ;
// END IOSF_NOT_PRESENT
    @(posedge primary_clock);
    release `MBY_TOP_PATH.prim_pok;
    force `MBY_TOP_PATH.prim_pok = 1;
end

initial begin
    force `MBY_TOP_PATH.sb_pok = 0;
    @(posedge mby_if.secondary_reset) ;
    // @(posedge secondary_clock);
    release `MBY_TOP_PATH.sb_pok;
    force `MBY_TOP_PATH.sb_pok = 1;
end

always #5000 primary_clock = (mby_if.enable_primary_clock ? ~primary_clock : 0);
always #5000 secondary_clock = (mby_if.enable_secondary_clock ?~secondary_clock : 0);

// START IOSF_NOT_PRESENT
//  //Primary clock gating
//  always @(posedge primary_clock) begin
//    if (idle_conter < 10 && iosf_pri_if.prim_clkack === 0 && iosf_pri_if.prim_clkreq === 0)
//      idle_conter +=  1;
//    else
//      if (iosf_pri_if.prim_clkack !== 0 || iosf_pri_if.prim_clkreq !== 0 ) begin
// idle_conter = 0;
//      end
//  end
// END IOSF_NOT_PRESENT

always @(primary_clock) begin
    if (idle_conter > 8) begin
        gated_primary_clock  <= 0;
    end else begin
        gated_primary_clock <=  primary_clock;
    end
end

// ===============================================
// Reset block instance
// ===============================================

// ===============================================
// INSTANCES
// ===============================================
// MBY controller instance + signal connections
// ===============================================

// Connecting clocks to RTL
// ===============================================

assign mby_primary_clock = gated_primary_clock;
assign mby_secondary_clock = gated_secondary_clock;

// Primary IOSF connection to MBY
// ===============================================

// START IOSF_NOT_PRESENT
//    // Inputs
//
//    assign `MBY_TOP_PATH.psf_mby_cmd_chid         = iosf_pri_if.cmd_chid;
//    assign `MBY_TOP_PATH.psf_mby_cmd_put          = iosf_pri_if.cmd_put;
//    assign `MBY_TOP_PATH.psf_mby_cmd_rtype        = iosf_pri_if.cmd_rtype;
//    assign `MBY_TOP_PATH.psf_mby_gnt              = iosf_pri_if.gnt;
//    assign `MBY_TOP_PATH.psf_mby_gnt_chid         = iosf_pri_if.gnt_chid;
//    assign `MBY_TOP_PATH.psf_mby_gnt_rtype        = iosf_pri_if.gnt_rtype;
//    assign `MBY_TOP_PATH.psf_mby_gnt_type         = iosf_pri_if.gnt_type;
//    assign `MBY_TOP_PATH.psf_mby_prim_clkack      = iosf_pri_if.prim_clkack;
//    assign `MBY_TOP_PATH.psf_mby_prim_ism_fabric  = iosf_pri_if.prim_ism_fabric;
//    assign `MBY_TOP_PATH.psf_mby_taddress         = iosf_pri_if.taddress;
//    assign `MBY_TOP_PATH.psf_mby_tdata            = iosf_pri_if.tdata;
//    assign `MBY_TOP_PATH.psf_mby_tdest_id         = iosf_pri_if.tdest_id;
//    assign `MBY_TOP_PATH.psf_mby_tfbe             = iosf_pri_if.tfbe;
//    assign `MBY_TOP_PATH.psf_mby_tfmt             = iosf_pri_if.tfmt;
//    assign `MBY_TOP_PATH.psf_mby_tlbe             = iosf_pri_if.tlbe;
//    assign `MBY_TOP_PATH.psf_mby_tlength          = iosf_pri_if.tlength;
//    assign `MBY_TOP_PATH.psf_mby_tns              = iosf_pri_if.tns;
//    assign `MBY_TOP_PATH.psf_mby_tro              = iosf_pri_if.tro;
//    assign `MBY_TOP_PATH.psf_mby_trqid            = iosf_pri_if.trqid;
//    assign `MBY_TOP_PATH.psf_mby_ttag             = iosf_pri_if.ttag;
//    assign `MBY_TOP_PATH.psf_mby_ttc              = iosf_pri_if.ttc;
//    assign `MBY_TOP_PATH.psf_mby_ttype            = iosf_pri_if.ttype;
//    assign `MBY_TOP_PATH.psf_mby_tsai             = iosf_pri_if.tsai;
//    //outputs
//    assign iosf_pri_if.credit_chid     = `MBY_TOP_PATH.mby_psf_credit_chid ;
//    assign iosf_pri_if.credit_cmd      = `MBY_TOP_PATH.mby_psf_credit_cmd ;
//    assign iosf_pri_if.credit_data     = `MBY_TOP_PATH.mby_psf_credit_data ;
//    assign iosf_pri_if.credit_put      = `MBY_TOP_PATH.mby_psf_credit_put ;
//    assign iosf_pri_if.credit_rtype    = `MBY_TOP_PATH.mby_psf_credit_rtype ;
//    assign iosf_pri_if.maddress        = `MBY_TOP_PATH.mby_psf_maddress ;
//    assign iosf_pri_if.mdata           = `MBY_TOP_PATH.mby_psf_mdata ;
//    assign iosf_pri_if.mfbe            = `MBY_TOP_PATH.mby_psf_mfbe ;
//    assign iosf_pri_if.mfmt            = `MBY_TOP_PATH.mby_psf_mfmt ;
//    assign iosf_pri_if.mlbe            = `MBY_TOP_PATH.mby_psf_mlbe ;
//    assign iosf_pri_if.mlength         = `MBY_TOP_PATH.mby_psf_mlength ;
//    assign iosf_pri_if.mns             = `MBY_TOP_PATH.mby_psf_mns ;
//    assign iosf_pri_if.mro             = `MBY_TOP_PATH.mby_psf_mro ;
//    assign iosf_pri_if.mrqid           = `MBY_TOP_PATH.mby_psf_mrqid ;
//    assign iosf_pri_if.mtag            = `MBY_TOP_PATH.mby_psf_mtag ;
//    assign iosf_pri_if.mtc             = `MBY_TOP_PATH.mby_psf_mtc ;
//    assign iosf_pri_if.mtype           = `MBY_TOP_PATH.mby_psf_mtype ;
//    assign iosf_pri_if.prim_clkreq     = `MBY_TOP_PATH.mby_psf_prim_clkreq ;
//    assign iosf_pri_if.prim_ism_agent  = `MBY_TOP_PATH.mby_psf_prim_ism_agent ;
//    assign iosf_pri_if.req_cdata       = `MBY_TOP_PATH.mby_psf_req_cdata ;
//    assign iosf_pri_if.req_chid        = `MBY_TOP_PATH.mby_psf_req_chid ;
//    assign iosf_pri_if.req_dlen        = `MBY_TOP_PATH.mby_psf_req_dlen ;
//    assign iosf_pri_if.req_locked      = `MBY_TOP_PATH.mby_psf_req_locked ;
//    assign iosf_pri_if.req_ns          = `MBY_TOP_PATH.mby_psf_req_ns ;
//    assign iosf_pri_if.req_put         = `MBY_TOP_PATH.mby_psf_req_put ;
//    assign iosf_pri_if.req_ro          = `MBY_TOP_PATH.mby_psf_req_ro ;
//    assign iosf_pri_if.req_rtype       = `MBY_TOP_PATH.mby_psf_req_rtype ;
//    assign iosf_pri_if.req_tc          = `MBY_TOP_PATH.mby_psf_req_tc ;
//    assign iosf_pri_if.msai            = `MBY_TOP_PATH.mby_psf_msai;
//
//    assign iosf_pri_if.msrc_id         = `MBY_TOP_PATH.mby_psf_msrc_id;
//    assign iosf_pri_if.mdest_id        = `MBY_TOP_PATH.mby_psf_mdest_id;
//    assign iosf_pri_if.req_dest_id     = `MBY_TOP_PATH.mby_psf_req_dest_id;
//
//
//  assign   iosf_pri_if.prim_clk               = primary_clock;
//  assign   iosf_pri_if.powergood_rst_b        = mby_if.power_good_reset;
//  assign   iosf_pri_if.prim_rst_b             = mby_if.primary_reset;
//  assign   iosf_pri_if.msecondary_bus_rst_b   = 1;//INTEG : connect to rtl if supported
//  assign   iosf_pri_if.tsecondary_bus_rst_b   = 1;//INTEG : connect to rtl if supported
//
//  //Unused
//
//    assign iosf_pri_if.mth = 0;
//   assign iosf_pri_if.mido = 0;
//   assign iosf_pri_if.mtd   =0;
//    assign iosf_pri_if.mecrc =0;
//
//    assign iosf_pri_if.mrsvd1_7     =0;
//    assign iosf_pri_if.mrsvd1_3     =0;
//    assign iosf_pri_if.mrsvd1_1     =0;
//    assign iosf_pri_if.mrsvd0_7     =0;
//    assign iosf_pri_if.mcparity     =0;
//    assign iosf_pri_if.mdparity     =0;
//
//    assign iosf_pri_if.req_ido        =0;
//    assign iosf_pri_if.req_chain      =0;
//    assign iosf_pri_if.req_opp        =0;
//    assign iosf_pri_if.req_agent      =0;
//    assign iosf_pri_if.req_priority   =0;
//    assign iosf_pri_if.sub_hit =0;
//    assign iosf_pri_if.hit =0;
// END IOSF_NOT_PRESENT

//INTEG : remove while real signals are connected :
assign   `MBY_TOP_PATH.mby_psf_mtag  = 0;
assign   `MBY_TOP_PATH.mby_psf_mep = 0;
assign   `MBY_TOP_PATH.mby_psf_mat = 0;
assign   `MBY_TOP_PATH.mby_psf_msai = 0;
assign   `MBY_TOP_PATH.mby_psf_mdest_id = 0;
assign   `MBY_TOP_PATH.mby_psf_msrc_id = 0 ;
// START IOSF_NOT_PRESENT
//  assign   iosf_pri_if.req_id        =0;
// END IOSF_NOT_PRESENT
assign   `MBY_TOP_PATH.mby_psf_req_dest_id = 0;
//signals for 2012WW46r121114 primary IOSF compliance
//INTEG : remove if real signals are connected
// START IOSF_NOT_PRESENT
//  assign   iosf_pri_if.mdbe           = 0;//Mas Data Byte Enables
//  assign   iosf_pri_if.mbewd          = 0;//Mas Byte En with Data
//  assign   iosf_pri_if.mecrc_generate = 0;//ECRC Generate
//  assign   iosf_pri_if.mecrc_error    = 0;//ECRC Error
//  assign   iosf_pri_if.mrs            = 0;//Root Space of address
//  assign   iosf_pri_if.req_rs         = 0;//Request Root_Space
//
//
//  assign   iosf_pri_if.prim_pok = `MBY_TOP_PATH.prim_pok; //for r130510
//  assign   iosf_pri_if.mep      = `MBY_TOP_PATH.mby_psf_mep;
//  assign   iosf_pri_if.mat      = `MBY_TOP_PATH.mby_psf_mat;
//
//  //for r131011
//  //  INTEG : remove if real signals are connected
//  assign   iosf_pri_if.mdeadline = 0;
//
//
//    // Secondary IOSF connection to MBY
//    // ===============================================
//
//    // Inputs
//
//    assign `MBY_TOP_PATH.sb2_mby_eom              = iosf_sb_if.meom;
//    assign `MBY_TOP_PATH.sb2_mby_npcup            = iosf_sb_if.tnpcup;
//    assign `MBY_TOP_PATH.sb2_mby_npput            = iosf_sb_if.mnpput;
//    assign `MBY_TOP_PATH.sb2_mby_payload          = iosf_sb_if.mpayload;
//    assign `MBY_TOP_PATH.sb2_mby_pccup            = iosf_sb_if.tpccup;
//    assign `MBY_TOP_PATH.sb2_mby_pcput            = iosf_sb_if.mpcput;
//    assign `MBY_TOP_PATH.sb2_mby_side_clkack      = iosf_sb_if.side_clkack;
//    assign `MBY_TOP_PATH.sb2_mby_side_ism_fabric  = iosf_sb_if.side_ism_fabric;
//
//   //INTEG : replace with CCU VC ack
//  assign   iosf_sb_if.side_clkack = 1;
//
//    //outputs
//    assign iosf_sb_if.teom     = `MBY_TOP_PATH.mby_sb2_eom ;
//    assign iosf_sb_if.mnpcup   = `MBY_TOP_PATH.mby_sb2_npcup ;
//    assign iosf_sb_if.tnpput   = `MBY_TOP_PATH.mby_sb2_npput ;
//    assign iosf_sb_if.tpayload = `MBY_TOP_PATH.mby_sb2_payload ;
//    assign iosf_sb_if.mpccup   = `MBY_TOP_PATH.mby_sb2_pccup ;
//    assign iosf_sb_if.tpcput   = `MBY_TOP_PATH.mby_sb2_pcput ;
//    assign iosf_sb_if.side_clkreq = `MBY_TOP_PATH.mby_sb2_side_clkreq ;
//    assign   iosf_sb_if.side_ism_agent = `MBY_TOP_PATH.mby_sb2_side_ism_agent ;
//    assign   iosf_sb_if.side_pok = `MBY_TOP_PATH.sb_pok;
// END IOSF_NOT_PRESENT

// Instance MBY ENV interface
// This is done in the TB if the IP need to drive also signals.
// ===============================================
mby_env_if mby_if();
assign mby_power_good_reset = mby_if.power_good_reset;
assign mby_secondary_reset = mby_if.secondary_reset;
assign mby_primary_reset = mby_if.primary_reset;
assign mby_if.primary_clock = primary_clock;
assign mby_if.secondary_clock = secondary_clock;
assign mby_if.mby_int_wire = `MBY_TOP.mby_int;


// ===============================================
// Test Island instance
// ===============================================
mby_ti_high #(

// START CHASSIS_NOT_PRESENT
//             .MBY_HAS_RESET_PKG(1),
// END CHASSIS_NOT_PRESENT
// START IOSF_NOT_PRESENT
//      .MBY_IOSF_IS_PASSIVE(0),
// END IOSF_NOT_PRESENT
// START CHASSIS_NOT_PRESENT
//      .RESET_PKG_IS_ACTIVE(1)
// END CHASSIS_NOT_PRESENT
)
u_mby_ti_high (
// START IOSF_NOT_PRESENT
//                     .mby_iosf_pri_if     (iosf_pri_if),
//                     .mby_iosf_sb_if      (iosf_sb_if),
// END IOSF_NOT_PRESENT
    .mby_if          (mby_if)
// START CHASSIS_NOT_PRESENT
//                     ,.pg_if (pg_if),
//                     .ccu_if(ccu_if)
// END CHASSIS_NOT_PRESENT
);

mby_wm_top mby_wm();

dummy_dut dummy_dut_();

// START IOSF_NOT_PRESENT
//initial begin
//  //INTEG : remove this code :
//  $assertoff(0, mby_tb.dummy_dut_.iosf_primary_ti.genblk1.primary_compmon.agent_ism_compliance.ISMPM_002_ISM_Initialization_With_AGENT_IDLE.ISMPM_002_ISM_Initialization_With_AGENT_IDLE_ASSERT);
//  $assertoff(0, mby_tb.u_mby_ti_high.u_mby_ti_low.mby_iosf_primary_ti.genblk1.primary_compmon.agent_ism_compliance.ISMPM_002_ISM_Initialization_With_AGENT_IDLE.ISMPM_002_ISM_Initialization_With_AGENT_IDLE_ASSERT);
//end
// END IOSF_NOT_PRESENT

///----------------------------------
//INTEG : PUT YOUR CODE HERE : replace wiht your power supplies
//INTEG : this is is only example , please replace with your power domains
/* -----\/----- EXCLUDED -----\/-----

 initial begin
 vccRail = 1'bx;
 #100ps;
 vccRail  = 1'b1;
 end

 initial
 begin: startCorruption
 $display("%m: Initial all the supply rails @ %0t", $time);
 if (UPF_SIMULATIONS)
 begin
 if (vccRail === 1'bx) //INTEG : replace with vccIF
 supply_off("vcccorevid_1p03");
 end
 end:startCorruption

 // PRIMARY Supply Rail
 always @(vccRail)
 begin: connectRail
 $display("%m: SCC Rail Triggered @ %0t", $time);
 // Only enable the connections for UPF simulations.
 // This code will not work for non-UPF based simulations
 if (UPF_SIMULATIONS)
 begin
 if (vccRail == 1'b1)
 begin
 supply_on("vcccorevid_1p03", 1.0); //INTEG : replace with your voltage
 end
 else if (vccRail == 1'b0)
 begin
 supply_off("vcccorevid_1p03");
 end
 else if (vccRail === 1'bx)
 begin
 supply_off("vcccorevid_1p03");
 end
 end
 end: connectRail

 -----/\----- EXCLUDED -----/\----- */


//////////////////////////////////////////////
// Hierarchy-Based RTL File List Dumping ////
/////////////////////////////////////////////

`include  "std_ace_util.vic"

initial begin
    dump_hier();
end // initial begin

initial begin
    dump_vcd();
end

initial begin
    dump_fsdb();
end

initial begin
    if ($test$plusargs("vpd")) begin                                      // Plusarg to Enable VPD dump of all signals in the design
        $vcdpluson();
    end

    if($test$plusargs("fsdb")) begin                                      // Plusarg to enable FSDB dump
        if ($value$plusargs("FSDB_ON=%s",str)) begin                       // Plusarg to enable a start sample time after a delay
            fsdb_on = convert_time(str);                                    // Time to start (Converted time via plusarg)
            $display("FSDB DUMP:  Waiting %s before starting fsdb dump", str);
            #fsdb_on;                                                       // Wait - time before start
        end else begin
            fsdb_on = 0;                                                    // Time to start (no specified Time, so start at 0)
        end

        if ($value$plusargs("FSDB_SIZE=%d",fsdb_file_size)) begin               // Plusarg to specify a FSDB dump file size Limit
            if (fsdb_file_size<32) begin                                         // Must be at least 32
                fsdb_file_size = 32;
            end
        end

        $value$plusargs("FSDB_FILE=%s",fsdb_file);                         // Plusarg to specify user defined FSDB dump file name

        $fsdbAutoSwitchDumpfile(fsdb_file_size,fsdb_file,0,{fsdb_file,"_vf.log"}); // Enablement of Auto file switching

        if ($test$plusargs("fsdb_config")) begin                           // Plusarg to indicate a FSDB.dump.config file will be used. It indicates which modules to sample.
            $fsdbDumpvarsToFile ("fsdb.dump.config");
        end else begin
            $fsdbDumpvars(0,mby_tb,"+all");                                 // Default - FSDB dump of all signals in the design
        end

        if ($value$plusargs("FSDB_OFF=%s",str)) begin                      // Plusarg to Enable a stop time of sampling
            fsdb_off = convert_time(str) - fsdb_on;                         // Overall Time to stop, not a length of time. (Converted time via plusarg, subtract Start time)
            $display("FSDB DUMP:  Stopping FSDB dump in %s", str);
            if (fsdb_off>0) begin                                           // calculated difference must be greater than 0, else stop imediately
                #fsdb_off;                                                   // Wait - time before stop
            end
            $fsdbDumpoff();                                                 // Turn off FSDB dumping
            $display("FSDB DUMP :  Stopped FSDB dump");
            fsdb_on = -1; //fsdb is off
        end
    end // FSDB dump
end

function automatic longint convert_time(string in);
    longint out = in.atoi();

    case (in.substr(in.len()-2, in.len()-1))
        "ms": out *= 1000000000; // ONE BILLION ps steps in a ms unit
        "us": out *= 1000000;    // ONE MILLION ps steps in a us unit
        "ns": out *= 1000;       // ONE THOUSAND ps steps in a ns unit
    endcase //case suffix

    return out;
endfunction //convert_time()

endmodule // mby_tb
