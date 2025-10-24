#!/usr/intel/bin/tcsh -f

cd $CONFIG_GEN
set __fpngen_ward=fp_leaf_netlist
cd $__fpngen_ward


# source /p/com/env/psetup/prod/bin/setupTool designcompiler I-2013.12-SP4
source $REPO_ROOT/collage/patches/setup_dc_env

dc_shell -f $REPO_ROOT/collage/patches/gen_upf_supply_net_info.tcl

# Remove temporary files from DC
/bin/rm *.pvl
/bin/rm *.syn
/bin/rm *.mr
/bin/rm default.svf



