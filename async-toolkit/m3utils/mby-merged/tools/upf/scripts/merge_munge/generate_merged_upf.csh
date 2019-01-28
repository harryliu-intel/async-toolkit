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
##  MBY
##------------------------------------------------------------------##
echo  "\n ---> Processing MBY ...." |& tee -a ${LOG_FILE_LOCATION}

cd $UPF_WORK_DIR/merge_results
if (-d soc) then
    rm -rf soc
endif
mkdir soc
cd soc
cp -fr $UPF_WORK_DIR/soc* .


setenv SPYGLASS_LP_RUN 0
setenv ENABLE_SLP_RUN 0
echo "--->---> sub processing SOC .... Running for IMPL ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf soc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_soc.tcl -inc_file -tag impl -set_vars "synopsys_program_name dc_shell" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv SPYGLASS_LP_RUN
unsetenv ENABLE_SLP_RUN

setenv ENABLE_NLP_RUN 1
echo "--->---> sub processing SOC .... Running for NLP ....." |& tee -a ${LOG_FILE_LOCATION}
${MM_SCRIPT_ROOT}/merge_upf/merge_upf.tcl -upf soc.upf -verbose -config $UPF_SCRIPTS_DIR/merge_munge/merge_upf_config_soc.tcl -inc_file -tag sim -set_vars "synopsys_program_name vcs" -skip_load_upfs "par*" |& tee -a ${LOG_FILE_LOCATION}
unsetenv ENABLE_NLP_RUN

##Cleanup load upf paths
sed -s -i 's;source $::env(UPF_INSTALL_DIR);source $::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' soc.*upf
sed -s -i '/if {\[info exists synopsys_program_name\] && ($synopsys_program_name == "vcs")} {/d' soc.*upf
sed -s -i 's;\s*/nfs\s*\S*/soc/upf/outputs;$::env(MODEL_ROOT)/target/mby/upf/gen/fc_par;g' soc.*upf
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

