#!/usr/intel/bin/tcsh -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

setenv MODEL_ROOT `pwd`
eval `/p/hdk/rtl/proj_tools/onecfg/master/1.02.07/bin/ToolSetup.pl vcsmx`
cd $MODEL_ROOT/subBlock/mbyc/src/egr/sandbox
make gate 
grep -E 'Error|Offending' build.log gate.log
if ( $status != 1 ) then
  echo "Error: EGR Sandbox Testbench failed. see the log files subBlock/mbyc/src/egr/sandbox/{build,gate}.log"
  cd $MODEL_ROOT
  unsetenv MODEL_ROOT
  exit 1;
endif
cd $MODEL_ROOT
unsetenv MODEL_ROOT
