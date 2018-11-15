#!/bin/sh
cd ../rtl
SHAREDDIR="../../shared/"
GMDIR="../../rtl/gm/"
vcs -full64 +v2k -sverilog -notice +vcs+lic+wait +warn=noUII-L \
    -timescale=10ps/10ps -debug_pp +libext+.v+.sv+.vh+.svh+.vs+.vm+.sva \
    -assert svaext +define+VCSSIM +define+NO_SLA $SHAREDDIR/rtl/shared_pkg.sv \
    $SHAREDDIR/rtl/mby_gmm_pkg.sv $SHAREDDIR/rtl/mby_egr_pkg.sv \
    ../interfaces/*.sv \
    $SHAREDDIR/interfaces/egr_epl_if.sv \
    $SHAREDDIR/interfaces/egr_tx_ppe_if.sv \
    $SHAREDDIR/interfaces/mim_rd_if.sv \
    $SHAREDDIR/interfaces/mim_wr_if.sv \
    $SHAREDDIR/interfaces/egr_ppe_stm_if.sv \
    $SHAREDDIR/interfaces/egr_igr_if.sv \
    egr_top.sv ../sandbox/*.sv
cd ../sandbox
