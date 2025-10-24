#!/usr/intel/bin/tcsh -f


## This setup is needed to run RDT FC
setenv POC_PD_REPO /nfs/pdx/disks/srvr10nm_0498/snkalval
setenv KIT_PATH /p/kits/intel/p1273/p1273_1.3.0
setenv LIBRARY_TYPE d04
setenv DESIGN_NAME soc
source $KIT_PATH/kit_setup

setenv CONFIG_GEN $COLLAGE_WORK/gen

# Creating work areas
mkdir -p $COLLAGE_WORK/POC_PD_WORK
mkdir -p $COLLAGE_WORK/POC_PD_WORK/fc
mkdir -p $COLLAGE_WORK/POC_PD_WORK/fc/inputs
mkdir -p $COLLAGE_WORK/POC_PD_WORK/fc/scripts
mkdir -p $COLLAGE_WORK/POC_PD_WORK/lattice

## Script which contains the utils to generate the be collateral
  ## /nfs/pdx/disks/srvr10nm_0498/snkalval/POC_PD_REPO/utils/collage_begen_utils.tcl

## Input config file to Collage to generate the be collaterals
  ## /nfs/pdx/disks/srvr10nm_0498/snkalval/POC_PD_REPO/utils/soc_be_test_config.tcl


## Assumption is that the User has already generated top level vg file and the be collaterals based on the be_config file in the RTL assembly step

# Preparing inputs for different flows

## Copying canned inputs from REPO area, User can copy these canned scripts into integ_specs area and get rid of the POC_PD_REPO area
cp -r $POC_PD_REPO/POC_PD_REPO/FC/scripts/tb_pg_template.tpl $COLLAGE_WORK/POC_PD_WORK/fc/scripts/
cp -r $POC_PD_REPO/POC_PD_REPO/FC/scripts/apr_procs.tcl $COLLAGE_WORK/POC_PD_WORK/fc/scripts/

## Copying the generated outputs based on config
cp -r $CONFIG_GEN/fc/soc_macro_placement.tcl $COLLAGE_WORK/POC_PD_WORK/fc/scripts/
cp -r $CONFIG_GEN/fc/block_setup.tcl $COLLAGE_WORK/POC_PD_WORK/fc/scripts/
cp -r $CONFIG_GEN/fc/soc_bbox_area.tcl $COLLAGE_WORK/POC_PD_WORK/fc/inputs/

## Copying the top level netlist out of Collage for FC
echo "// Top level netlist" >! $COLLAGE_WORK/POC_PD_WORK/fc/inputs/soc.vg
cat $CONFIG_GEN/stubs/floorplan/* >> $COLLAGE_WORK/POC_PD_WORK/fc/inputs/soc.vg
cat $CONFIG_GEN/fp_unit_netlist/outputs/soc_only.vg >> $COLLAGE_WORK/POC_PD_WORK/fc/inputs/soc.vg

## Copying Lattice inputs from POC_PD_REPO area, the updated Lattice package has been kept under LATTICE directory of POC_PD_REPO area if the User wishes to move the package to a different area then the run_lattice.tcl file should be updated to pick up the package from new area.
cp -r $POC_PD_REPO/POC_PD_REPO/LATTICE/ltc_config_settings.tcl $COLLAGE_WORK/POC_PD_WORK/lattice/

# Running FC to generate DB for Lattice
cd $COLLAGE_WORK/POC_PD_WORK/fc/
icc_shell -dp_mode -x "source $KIT_PATH/flows/rdt/common/scripts/run.tcl;runRDT -init -stop power_grid_fc_plan;exit"


cd $COLLAGE_WORK/POC_PD_WORK/lattice/
icc_shell -dp_mode -x  "lappend auto_path $POC_PD_REPO/POC_PD_REPO/LATTICE; package require LatticeRoute; set G_LIBRARY_TYPE d04; source $KIT_PATH/flows/rdt/common/scripts/run.tcl; open_mw_lib ../fc/dbs/soc_power_grid_fc_plan_LIB; open_mw_cel soc ; ltc_track_plan -config_settings_file ./ltc_config_settings.tcl -track_plan -detail_route -repeater_insertion -generate_timing -verbose; exit"

