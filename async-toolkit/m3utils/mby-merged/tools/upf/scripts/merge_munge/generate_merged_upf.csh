#!/bin/csh
##--------------------------------------------------------------------------##
## Merges partition level UPF files which have load_upf commands to create a 
## flat partition level UPF file to be loaded into simulation and implementation
## NOTE: This script is specific to DLC. This is not a generic script
## Author: rjstach
##--------------------------------------------------------------------------##

## Need to unalias cp as in some cases cp is being set to cp -ip
## This especially was seen in cases where gnome terminal was involved
## Below unaliasing would be the cleanest as it does not impact the shell
unalias cp
unalias rm
unalias mv
unalias grep

## All contents of the merge will be written into ${LOG_FILE_LOCATION}

set LOG_FILE_LOCATION = "${MODEL_ROOT}/target/mby/collage/work/soc/upf/logs/log.merge"
if (-e ${LOG_FILE_LOCATION}) then
    set lastlog = `stat -c %y ${LOG_FILE_LOCATION} | sed -e 's/[-: ]//g' -e 's/..\..*//'`
    mv ${LOG_FILE_LOCATION} ${LOG_FILE_LOCATION}.$lastlog
    # rm -f ${LOG_FILE_LOCATION}
endif

touch ${LOG_FILE_LOCATION}

## Initial logistics to get the script going

set MM_SCRIPT_ROOT = "$COLLAGE_ROOT/comps/upf"
echo "Using $MM_SCRIPT_ROOT/merge_upf/merge_upf.tcl for merging" |& tee -a ${LOG_FILE_LOCATION}
echo "Using $MM_SCRIPT_ROOT/munge_upf/munge_upf.tcl for munging" |& tee -a ${LOG_FILE_LOCATION}

setenv UPF_INSTALL_DIR ${MODEL_ROOT}/target/mby/collage/work/soc/upf/outputs 
setenv UPF_WORK_DIR "$UPF_INSTALL_DIR"
echo "UPF_WORK_DIR: $UPF_WORK_DIR" |& tee -a ${LOG_FILE_LOCATION}

setenv UPF_SCRIPTS_DIR ${MODEL_ROOT}/tools/upf/scripts
echo "UPF_SCRIPTS_DIR: $UPF_SCRIPTS_DIR" |& tee -a ${LOG_FILE_LOCATION}

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
## Merging process starts from here
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
cd $UPF_WORK_DIR

if (-d merge_results) then
    rm -rf merge_results
endif

mkdir merge_results

if (-d $MODEL_ROOT/target/mby/upf/gen/fc_par) then
    rm -rf $MODEL_ROOT/target/mby/upf/gen/fc_par
endif

mkdir -p $MODEL_ROOT/target/mby/upf/gen/fc_par
setenv UPF_FINAL_DIR "$MODEL_ROOT/target/mby/upf/gen/fc_par"

##------------------------------------------------------------------##
## PARACC
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACC ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracc) then
    rm -rf paracc
endif
mkdir paracc
cd paracc
cp -fr $UPF_WORK_DIR/paracc* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACC .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracc.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACC .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracc.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracc.*upf
sed -s -i '/if {\[info exists synopsys_program_name\] && ($synopsys_program_name == "vcs")} {/d' paracc.*upf
sed -s -i 's;\s*/nfs\s*\S*/paracc\/;$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par/;g' paracc.*upf
sed -s -i '/load_upf "$::env(MODEL_ROOT)\/target\/mby\/upf\/gen\/fc_par\/paracctemp8.upf" -scope "paracctemp8"/,/}/c load_upf "$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par/paracctemp8.upf" -scope "paracctemp8"' paracc.*upf

##Change upf versions to "sim" for paracc.sim.upf
sed -i 's;\.upf;\.sim.upf;g' paracc.sim.upf

cp -f paracc.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracc.upf
cp -f paracc.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracc.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracc.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCCLK
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCCLK ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paraccclk) then
    rm -rf paraccclk
endif
mkdir paraccclk
cd paraccclk
cp -fr $UPF_WORK_DIR/paraccclk* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCCLK .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paraccclk.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paraccclk.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCCLK .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paraccclk.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paraccclk.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paraccclk.*upf

cp -f paraccclk.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paraccclk.upf
cp -f paraccclk.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paraccclk.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paraccclk.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCCSS
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCCSS ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracccss) then
    rm -rf paracccss
endif
mkdir paracccss
cd paracccss
cp -fr $UPF_WORK_DIR/paracccss* .
cp -fr $UPF_WORK_DIR/acc_etf_* .
cp -fr $UPF_WORK_DIR/apb* .
cp -fr $UPF_WORK_DIR/cdc* .
cp -fr $UPF_WORK_DIR/time_stamp* .
cp -fr $UPF_WORK_DIR/trace_router* .
cp -fr $UPF_WORK_DIR/trig_router* .


setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCCSS .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracccss.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracccss.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCCSS .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracccss.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracccss.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracccss.*upf

##Change element list to variable for the include file 
sed -s -i 's;create_power_domain "vnn_island_paracccss_pd_UNGATED_0" -elements {VNN_ISLAND_PARACCCSS_PD_UNGATED_0_ELEMENTS};create_power_domain "vnn_island_paracccss_pd_UNGATED_0" -elements $VNN_ISLAND_PARACCCSS_PD_UNGATED_0_ELEMENTS;g' paracccss.*upf

cp -f paracccss.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracccss.upf
cp -f paracccss.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracccss.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracccss.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCDFX
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCDFX ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paraccdfx) then
    rm -rf paraccdfx
endif
mkdir paraccdfx
cd paraccdfx
cp -fr $UPF_WORK_DIR/paraccdfx* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCDFX .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paraccdfx.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paraccdfx.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCDFX .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paraccdfx.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paraccdfx.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paraccdfx.*upf

cp -f paraccdfx.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paraccdfx.upf
cp -f paraccdfx.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paraccdfx.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paraccdfx.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP0
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP0 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp0) then
    rm -rf paracctemp0
endif
mkdir paracctemp0
cd paracctemp0
cp -fr $UPF_WORK_DIR/paracctemp0* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP0 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp0.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp0.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP0 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp0.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp0.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp0.*upf

cp -f paracctemp0.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp0.upf
cp -f paracctemp0.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp0.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp0.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP1
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP1 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp1) then
    rm -rf paracctemp1
endif
mkdir paracctemp1
cd paracctemp1
cp -fr $UPF_WORK_DIR/paracctemp1* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP1 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp1.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp1.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP1 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp1.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp1.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp1.*upf

cp -f paracctemp1.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp1.upf
cp -f paracctemp1.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp1.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp1.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP2
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP2 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp2) then
    rm -rf paracctemp2
endif
mkdir paracctemp2
cd paracctemp2
cp -fr $UPF_WORK_DIR/paracctemp2* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP2 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp2.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp2.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP2 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp2.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp2.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp2.*upf

cp -f paracctemp2.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp2.upf
cp -f paracctemp2.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp2.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp2.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP3
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP3 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp3) then
    rm -rf paracctemp3
endif
mkdir paracctemp3
cd paracctemp3
cp -fr $UPF_WORK_DIR/paracctemp3* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP3 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp3.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp3.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP3 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp3.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp3.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp3.*upf

cp -f paracctemp3.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp3.upf
cp -f paracctemp3.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp3.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp3.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP4
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP4 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp4) then
    rm -rf paracctemp4
endif
mkdir paracctemp4
cd paracctemp4
cp -fr $UPF_WORK_DIR/paracctemp4* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP4 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp4.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp4.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP4 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp4.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp4.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp4.*upf

cp -f paracctemp4.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp4.upf
cp -f paracctemp4.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp4.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp4.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP5
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP5 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp5) then
    rm -rf paracctemp5
endif
mkdir paracctemp5
cd paracctemp5
cp -fr $UPF_WORK_DIR/paracctemp5* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP5 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp5.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp5.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP5 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp5.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp5.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp5.*upf

cp -f paracctemp5.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp5.upf
cp -f paracctemp5.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp5.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp5.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP6
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP6 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp6) then
    rm -rf paracctemp6
endif
mkdir paracctemp6
cd paracctemp6
cp -fr $UPF_WORK_DIR/paracctemp6* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP6 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp6.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp6.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP6 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp6.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp6.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp6.*upf

cp -f paracctemp6.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp6.upf
cp -f paracctemp6.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp6.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp6.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP7
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP7 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp7) then
    rm -rf paracctemp7
endif
mkdir paracctemp7
cd paracctemp7
cp -fr $UPF_WORK_DIR/paracctemp7* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP7 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp7.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp7.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP7 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp7.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp7.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp7.*upf

cp -f paracctemp7.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp7.upf
cp -f paracctemp7.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp7.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp7.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARACCTEMP8
##------------------------------------------------------------------##

echo  "\n ---> Processing PARACCTEMP8 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d paracctemp8) then
    rm -rf paracctemp8
endif
mkdir paracctemp8
cd paracctemp8
cp -fr $UPF_WORK_DIR/paracctemp8* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARACCTEMP8 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp8.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp8.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARACCTEMP8 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf paracctemp8.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_paracctemp8.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' paracctemp8.*upf

cp -f paracctemp8.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/paracctemp8.upf
cp -f paracctemp8.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp8.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f paracctemp8.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARCLK
##------------------------------------------------------------------##

echo  "\n ---> Processing PARCLK ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parclk) then
    rm -rf parclk
endif
mkdir parclk
cd parclk
cp -fr $UPF_WORK_DIR/parclk* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARCLK .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parclk.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parclk.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARCLK .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parclk.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parclk.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parclk.*upf
sed -s -i '/if {\[info exists synopsys_program_name\] && ($synopsys_program_name == "vcs")} {/d' parclk.*upf
sed -s -i 's;\s*/nfs\s*\S*/parclk\/;$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par/;g' parclk.*upf
sed -s -i '/load_upf "$::env(MODEL_ROOT)\/target\/mby\/upf\/gen\/fc_par\/parclk.upf" -scope "parclk"/,/}/c load_upf "$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par/parclk.upf" -scope "parclk"' parclk.*upf


cp -f parclk.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parclk.upf
cp -f parclk.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parclk.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parclk.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.


##------------------------------------------------------------------##
## PARIMC
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMC ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimc) then
    rm -rf parimc
endif
mkdir parimc
cd parimc
cp -fr $UPF_WORK_DIR/parimc* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMC .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimc.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMC .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimc.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimc.*upf
sed -s -i '/if {\[info exists synopsys_program_name\] && ($synopsys_program_name == "vcs")} {/d' parimc.*upf
sed -s -i 's;\s*/nfs\s*\S*/parimc\/;$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par/;g' parimc.*upf
sed -s -i '/load_upf "$::env(MODEL_ROOT)\/target\/fc\/upf\/gen\/fc_par\/parimcsbbfuse.upf" -scope "parimcsbbfuse"/,/}/c load_upf "$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par/parimcsbbfuse.upf" -scope "parimcsbbfuse"' parimc.*upf

##Change upf versions to "sim" for parimc.sim.upf
sed -i 's;\.upf;\.sim.upf;g' parimc.sim.upf


cp -f parimc.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimc.upf
cp -f parimc.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimc.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimc.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARIMCA53
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMCA53 ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimca53) then
    rm -rf parimca53
endif
mkdir parimca53
cd parimca53
cp -fr $UPF_WORK_DIR/parimca53* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMCA53 .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimca53.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimca53.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMCA53 .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimca53.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimca53.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimca53.*upf

cp -f parimca53.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimca53.upf
cp -f parimca53.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimca53.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimca53.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARIMCNIC
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMCNIC ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimcnic) then
    rm -rf parimcnic
endif
mkdir parimcnic
cd parimcnic
cp -fr $UPF_WORK_DIR/parimcnic* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMCNIC .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcnic.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcnic.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMCNIC .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcnic.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcnic.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimcnic.*upf

cp -f parimcnic.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimcnic.upf
cp -f parimcnic.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcnic.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcnic.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.


##------------------------------------------------------------------##
## PARIMCCLK
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMCCLK ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimcclk) then
    rm -rf parimcclk
endif
mkdir parimcclk
cd parimcclk
cp -fr $UPF_WORK_DIR/parimcclk* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMCCLK .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcclk.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcclk.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMCCLK .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcclk.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcclk.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimcclk.*upf

cp -f parimcclk.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimcclk.upf
cp -f parimcclk.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcclk.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcclk.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARIMCDFXLEG
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMCDFXLEG ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimcdfxleg) then
    rm -rf parimcdfxleg
endif
mkdir parimcdfxleg
cd parimcdfxleg
cp -fr $UPF_WORK_DIR/parimcdfxleg* .
cp -fr $UPF_WORK_DIR/ETF_* .
cp -fr $UPF_WORK_DIR/apbic0* .
cp -fr $UPF_WORK_DIR/cdc_bridge_imc* .
cp -fr $UPF_WORK_DIR/debug_port* .
cp -fr $UPF_WORK_DIR/trace_router3* .
cp -fr $UPF_WORK_DIR/trig_router_imc* .
cp -fr $UPF_WORK_DIR/trig_router_noESB* .
cp -fr $UPF_WORK_DIR/ts_in_imc* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMCDFXLEG .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcdfxleg.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcdfxleg.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMCDFXLEG .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcdfxleg.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcdfxleg.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimcdfxleg.*upf

##Change -elements to point to variable instead of list
sed -s -i 's;create_power_domain "vnn_island_parimcdfxleg_pd_UNGATED_0" -elements {VNN_ISLAND_PARIMCDFXLEG_PD_UNGATED_0_ELEMENTS};create_power_domain "vnn_island_parimcdfxleg_pd_UNGATED_0" -elements $VNN_ISLAND_PARIMCDFXLEG_PD_UNGATED_0_ELEMENTS;g' parimcdfxleg.*upf

cp -f parimcdfxleg.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimcdfxleg.upf
cp -f parimcdfxleg.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcdfxleg.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcdfxleg.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARIMCGPIO
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMCGPIO ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimcgpio) then
    rm -rf parimcgpio
endif
mkdir parimcgpio
cd parimcgpio
cp -fr $UPF_WORK_DIR/parimcgpio* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMCGPIO .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcgpio.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcgpio.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMCGPIO .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcgpio.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcgpio.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimcgpio.*upf

cp -f parimcgpio.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimcgpio.upf
cp -f parimcgpio.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcgpio.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcgpio.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARIMCLSMGIC
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMCLSMGIC ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimclsmgic) then
    rm -rf parimclsmgic
endif
mkdir parimclsmgic
cd parimclsmgic
cp -fr $UPF_WORK_DIR/parimclsmgic* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMCLSMGIC .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimclsmgic.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimclsmgic.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMCLSMGIC .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimclsmgic.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimclsmgic.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimclsmgic.*upf

cp -f parimclsmgic.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimclsmgic.upf
cp -f parimclsmgic.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimclsmgic.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimclsmgic.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
## PARIMCSBBFUSE
##------------------------------------------------------------------##

echo  "\n ---> Processing PARIMCSBBFUSE ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d parimcsbbfuse) then
    rm -rf parimcsbbfuse
endif
mkdir parimcsbbfuse
cd parimcsbbfuse
cp -fr $UPF_WORK_DIR/parimcsbbfuse* .
cp -fr $UPF_WORK_DIR/ETF_* .

setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing PARIMCSBBFUSE .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcsbbfuse.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcsbbfuse.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell"  |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing PARIMCSBBFUSE .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf parimcsbbfuse.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_parimcsbbfuse.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' parimcsbbfuse.*upf

cp -f parimcsbbfuse.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/parimcsbbfuse.upf
cp -f parimcsbbfuse.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcsbbfuse.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f parimcsbbfuse.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.



##------------------------------------------------------------------##
##  MEV
##------------------------------------------------------------------##
echo  "\n ---> Processing MEV ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d soc) then
    rm -rf soc
endif
mkdir soc
cd soc
cp -fr $UPF_WORK_DIR/soc* .
cp -fr $UPF_WORK_DIR/paracc* .


setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing SOC .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf soc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_soc_TEMP.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing SOC .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf soc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_soc_TEMP.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' soc.*upf
sed -s -i '/if {\[info exists synopsys_program_name\] && ($synopsys_program_name == "vcs")} {/d' soc.*upf
sed -s -i 's;\s*/nfs\s*\S*/soc;$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' soc.*upf
sed -s -i '/load_upf "$::env(MODEL_ROOT)\/target\/fc\/upf\/gen\/fc_par\/parimc.upf" -scope "parimc"/,/}/c load_upf "$::env(MODEL_ROOT)\/target\/fc\/upf\/gen\/fc_par\/parimc.upf" -scope "parimc"' soc.*upf

##Change upf versions to "sim" for soc.sim.upf
sed -i 's;\.upf;\.sim.upf;g' soc.sim.upf


cp -f soc.impl.upf     $MODEL_ROOT/target/mby/upf/gen/fc_par/soc.upf
cp -f soc.inc.impl.tcl $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f soc.sim.upf      $MODEL_ROOT/target/mby/upf/gen/fc_par/.
cp -f soc.inc.sim.tcl  $MODEL_ROOT/target/mby/upf/gen/fc_par/.

##------------------------------------------------------------------##
##------------------------------------------------------------------##
##------------------------------------------------------------------##

cd $MODEL_ROOT
echo  "\n UPF Merge Done \n" |& tee -a ${LOG_FILE_LOCATION}



endofscript:

