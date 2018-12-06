//There will be a TB here
//
`timescale 1ns/10ps
`include "shared_pkg.sv"
`include "mby_gmm_pkg.sv"
`include "mby_egr_pkg.sv"
`include "egr_int_pkg.sv"
module prc_tb
import shared_pkg::*;
import mby_gmm_pkg::*;
import egr_int_pkg::*;
();


logic clk;
logic arst_n;

//EGR Internal Interfaces    
egr_dp_if             dpb_if(); //Dirty Pointer Interface. Requests from the Dirty Pointer Broker
egr_prc_lcm_if              lcm_if(); //Packet Read Controller - Local Congestion Manager Interface
egr_prc_tmu_if              tmu_if(); //Packet Read Controller - Tag Management Unit      Interface
egr_pfs_prc_if              pfs_if(); //Packet Fetch Scheduler - Packet Read Controller   Interface
egr_rrq_if#(.N_RREQS(2))           mri_if0(); //Read Request Interface.  Gives service to EPL 0,1
egr_rrq_if#(.N_RREQS(2))           mri_if1(); //Read Request Interface.  Gives service to EPL 2,3
egr_rrs_id_if rrs_id_mri_if0(); //Read Response ID Interface 0. Gives service to EPL0,1
egr_rrs_id_if rrs_id_mri_if1(); //Read Response ID Interface 1. Gives service to EPL2,3
egr_prc_tqu_if              tqu_if();  //Transmit Queuing Unit  - Packet Read Controller   Interface

prc dut_prc(.clk(clk),
            .rst_n(arst_n),
            .dpb_if(dpb_if),
            .lcm_if(lcm_if),
            .tmu_if(tmu_if),
            .pfs_if(pfs_if),
            .mri_if0(mri_if0),
            .mri_if1(mri_if1),
            .rrs_id_mri_if0(rrs_id_mri_if0),
            .rrs_id_mri_if1(rrs_id_mri_if1),
            .tqu_if(tqu_if)
           );

////////////////////////////////
//// SANDBOX RUNTIME ///////////
////////////////////////////////
mby_sandbox_runtime runtime(.clk(clk), .rst_n(arst_n));

default clocking clock @(posedge clk);
endclocking;

// PFS to PRC
initial begin

    wait(!arst_n);
    pfs_if.valid = '0;
    pfs_if.port  = '0;
    pfs_if.mgp   = '0;
    pfs_if.tc    = '0;
    pfs_if.dtq   = '0;

    wait(arst_n);
    ##(200);
    pfs_if.valid[0] = 1'b1;
    ##1;
    pfs_if.valid = '0;

end

// PRC to TMU
initial begin
    tmu_if.tag = '0;
    wait(pfs_if.valid);
    wait(!pfs_if.valid);
    ##(200);
    tmu_if.tag[0].valid = '1;
    tmu_if.tag[0].length = '1;
    tmu_if.tag[0].eop  = 1'b1;
    tmu_if.tag[0].ptr = $random;
    tmu_if.tag[0].ptr_toggle = $random;
    ##(1);
    tmu_if.tag[0].valid = 1'b0;
    tmu_if.tag[0].eop   = 1'b0;


end
endmodule: prc_tb
