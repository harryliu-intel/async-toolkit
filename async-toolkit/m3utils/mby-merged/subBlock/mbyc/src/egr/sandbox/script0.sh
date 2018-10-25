#!/bin/sh
cd ../rtl
SHAREDDIR="../../shared/"
GMDIR="../../rtl/gm/"
vcs -full64 +v2k -sverilog -notice +vcs+lic+wait +warn=noUII-L -timescale=10ps/10ps -debug_pp +libext+.v+.sv+.vh+.svh+.vs+.vm+.sva -assert svaext +define+VCSSIM +define+NO_SLA $SHAREDDIR/rtl/shared_pkg.sv $SHAREDDIR/rtl/mby_gmm_pkg.sv $SHAREDDIR/rtl/mby_egr_pkg.sv egr_igr_if.sv egr_ahb_if.sv egr_epl_if.sv egr_mc_table_if.sv egr_pod_if.sv egr_smm_readarb_if.sv egr_smm_writearb_if.sv egr_statsmgmt_if.sv egr_tagring_if.sv egr_mce_tagring_if.sv $SHAREDDIR/interfaces/egr_ppe_stm_if.sv $SHAREDDIR/interfaces/egr_tx_ppe_if.sv ../sandbox/egr_dummy_if.sv egr_top.sv ../sandbox/egr_top_tb.sv
cd ../sandbox
