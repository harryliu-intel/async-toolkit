#!/bin/csh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#
# run the minimum amount of bman necessary to enable emubuild with the transactors and SPARK TB, 
#  and only the first time the model is cloned, then run emubuild with the standard options
# Additional emubuild command line arguments can be passed to this script, some common ones are:
#   -target <something>  :   builds into an area different than the default
#   -clean_target        :   required if you are building into an area that has had previous build failures 
#                            (this is no longer the default so that we can do incremental and TB only recompiles)
#   -compile_substages tb -compile_substages-    :  this will redo only the TB compile stages, not skipping FE and BE
#   -s ZSE1_XTORS_anaylze_scripts                :  skip the given stage 
#                (These last two used together is nice for when you need to rebuild with changed versions of 
#                  GCC, SIMICS, SPARK modules, etc, anything that does not touch your RTL, and it takes 15min and 
#                  preserves your existing FE+BE synthesis)
#
unsetenv CONFIG_COMPILE_ROOT
unsetenv EMU_ROOT
unsetenv LD_LIBRARY_PATH
unsetenv SYNOPSYS_SIM_SETUP
unsetenv MODEL_ROOT
# check for a -target switch, as this requires special handling
@ i          = 1
@ targSwitch = 0
@ targArg    = 0
set model=""
while ($i <= $#argv) 
    if ("$argv[$i]" == "-model") then
        @ targSwitch = $i
        @ targArg    = $i + 1
        if ($targArg > $#argv) then
            echo "-model specified but no data  supplied as the next argument, exiting..."
            exit -11
        endif
        set targArgCharOne=`echo $argv[$targArg] | awk '{print substr($0,1,1)}'`
        if ("$targArgCharOne" == "-") then
            echo "the specified -model: $argv[$targArg]  begins with a -, which is most likely indicative of a missing argument to -model, exiting..."
            exit -12
        endif
        set model=$argv[$targArg]
        @ i = $#argv
    endif

# FIXME - Target doesn't work now that I put model option in. Figure this out later
#    if ("$argv[$i]" == "-target") then
#        @ targSwitch = $i
#        @ targArg    = $i + 1
#        if ($targArg > $#argv) then
#            echo "-target specified but no data  supplied as the next argument, exiting..."
#            exit -11
#        endif
#        set targArgCharOne=`echo $argv[$targArg] | awk '{print substr($0,1,1)}'`
#        if ("$targArgCharOne" == "-") then
#            echo "the specified -target: $argv[$targArg]  begins with a -, which is most likely indicative of a missing argument to -target, exiting..."
#            exit -12
#        endif
#        set target="target/emu/"$argv[$targArg]
#        @ i = $#argv
#    endif
    @ i++
end
# Wait for args to be read since model might change
set target="target/emu/$model/"

# Align correct yml with the model.
# VERY specific to MBY
if ($model == "mesh_emu") then
  set yml_tgt="buildProjectCfg.yml"
else
  echo "ERROR - Model $model is not a valid MBY emulation model. Must be mesh_emu"
  exit -12
endif

set platform=zse3
set ZSE_CFG=ZSE3_XTORS

# now we construct a string to pass to emubuild, aggregating all the input arguments besides -target <targ>
@ i = 1
set optionsString=""
while ($i <= $#argv) 
    if ($i != $targSwitch && $i != $targArg) then
        set optionsString="$optionsString $argv[$i]"
    endif
    @ i++
end
echo $optionsString

# CHECK #1 - Checking to see if we are running this script from the correct directory.
ls README >& /dev/null
if ( $status != 0)  then
    echo "no README file present, this does not seem like the root of an MBY model , exiting..."
    exit -1
endif

# CHECK #2 - Checking to see if we are running this script from the correct directory.
ls src >& /dev/null
if ( $status != 0)  then
    echo "no src/ directory present, this does not seem like the root of an MBY model , exiting..."
    exit -2
endif

# CHECK #3 - Checking to see if we are running this script from the correct directory.
ls tools/emu >& /dev/null
if ( $status != 0)  then
    echo "no tools/emu directory present, this does not seem like the root of an MBY model , exiting..."
    exit -3
endif

setenv MODEL_ROOT $PWD

# might want to be testing for collage, jem and stuff since that's what would drive us to run full bman --not necessarilly ace metadata
ls target/emu/$model/mby.M$model.filelist.Emulation.pl >& /dev/null 
if ( $status != 0) then
    echo "Simulation ACE metatdata does not exist, running bman to generate it."
    bman -mc $model -s emu -no_env_check -sched local
    if ( $status != 0) then
        echo "Error, bman -mc $model -s emu failed, exiting..."
        exit -5
    endif
endif

# emu_opts are being passed from the cfg/apr_hdl.udf for the various models.
ls target/emu/$model/mby.M$model.filelist.Emulation.pl >& /dev/null 
    if ( $status != 0) then
    echo "Emulation ACE metatdata does not exist, running bman's minimal emu stage to generate it."
    # emu_opts are being passed from the cfg/apr_hdl.udf for the various emulation models.
    bman -mc $model -s all  +s emu -no_env_check -sched local
    if ( $status != 0) then
        echo "Error, simbuild -dut $model -s all +s emu -aceopts "-emu_opts '-s all +s PRE_FLOW'" failed, exiting..."
        exit -4
    endif
endif

# echo "Running emubuild on the full SPARK TB configuration with all transactors"
# source target/emu/$model/emubuild_env.csh

# /nfs/site/eda/group/SYSNAME/emu/intel/emubuild/2.7.11/bin/emubuild -use_emubuild_tooldata -cfg $ZSE_CFG -cfg- -yml $MODEL_ROOT/tools/emu/cfg/$yml_tgt -plugins $MODEL_ROOT/tools/emu/plugins/compile -plugins- -target $target  $optionsString

# cp $MODEL_ROOT/src/rtl/rcf/sandbox/apr_rcf_top/efuse_data/soc_efuse.incr.hex $MODEL_ROOT/target/emu/$model/$platform/soc_efuse.hex
# cp $MODEL_ROOT/src/rtl/rcf/sandbox/apr_rcf_top/efuse_data/man_efuse.incr.hex $MODEL_ROOT/target/emu/$model/$platform/man_efuse.hex
# mkdir $MODEL_ROOT/target/emu/$model/$platform/boot_rom_code
# cp $MODEL_ROOT/boot_rom_code/irom.data $MODEL_ROOT/target/emu/$model/$platform/boot_rom_code/.
# cp $MODEL_ROOT/boot_rom_code/i2c_rom.data $MODEL_ROOT/target/emu/$model/$platform/boot_rom_code/.
# cp $MODEL_ROOT/boot_rom_code/i2c_rom_64.data $MODEL_ROOT/target/emu/$model/$platform/boot_rom_code/.

# echo "Changing target permissions"
# chmod 777 $MODEL_ROOT/target
# chgrp srvr10nm $MODEL_ROOT/target
# chmod 777 $MODEL_ROOT/target/emu
# chgrp srvr10nm $MODEL_ROOT/target/emu
# chmod 777 $MODEL_ROOT/target/emu/$model
# chgrp srvr10nm $MODEL_ROOT/target/emu/$model
# chmod -R 777 $MODEL_ROOT/target/emu/$model/$platform
# chgrp -R srvr10nm $MODEL_ROOT/target/emu/$model/$platform
# 
# sed '0,/setenv LD_LIBRARY_PATH/s//# setenv LD_LIBRARY_PATH/' $MODEL_ROOT/target/emu/$model/$platform/build_settings.csh > x
# mv x $MODEL_ROOT/target/emu/$model/$platform/build_settings.csh
# 
# # fixup the apr_emu_launch with the MODEL_ROOT and model info.
# sed -i -e "s@setenv MODEL_ROOT.*@setenv MODEL_ROOT $MODEL_ROOT@" ${MODEL_ROOT}/tools/emu/scripts/apr_emu_launch
# sed -i -e "s@set MODEL=.*@set MODEL=$model@" ${MODEL_ROOT}/tools/emu/scripts/apr_emu_launch
# 
# # fixup the run_itpp_test with the MODEL_ROOT and model info.
# sed -i -e "s@setenv MODEL_ROOT.*@setenv MODEL_ROOT $MODEL_ROOT@" ${MODEL_ROOT}/tools/emu/scripts/run_itpp_test
# sed -i -e "s@set MODEL=.*@set MODEL=$model@" ${MODEL_ROOT}/tools/emu/scripts/run_itpp_test
