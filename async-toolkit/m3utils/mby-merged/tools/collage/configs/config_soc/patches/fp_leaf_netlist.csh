#!/usr/intel/bin/tcsh -f

cd $CONFIG_GEN
set __fpngen_ward=fp_leaf_netlist
mkdir -p ${__fpngen_ward}/inputs
mkdir -p ${__fpngen_ward}/outputs
mkdir -p ${__fpngen_ward}/outputs/ddc/
cd $__fpngen_ward

# Default RTL locations
/bin/cp $CONFIG_GEN/source/rtl/*.sv inputs/
/bin/cp $CONFIG_GEN/stubs/fp_leaf_netlist/* inputs/

#/bin/cp $CONFIG_GEN/stubs/fp_netlist/* inputs/

# --------------------------------------
# Override stubs - hacks and workarounds
# --------------------------------------
#if (-e $CONFIG_GEN/stubs/verification/dfx_bist_tap.sv) then
#  /bin/cp $CONFIG_GEN/stubs/verification/dfx_bist_tap.sv inputs/
#endif


# source /p/com/env/psetup/prod/bin/setupTool designcompiler I-2013.12-SP4
source $REPO_ROOT/collage/patches/setup_dc_env

dc_shell -f $REPO_ROOT/collage/patches/gen_fp_leaf_netlist.tcl

# Remove temporary files from DC
/bin/rm *.pvl
/bin/rm *.syn
/bin/rm *.mr
/bin/rm default.svf



