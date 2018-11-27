#!/usr/intel/bin/tcsh -f
setenv MODEL_ROOT `pwd`
eval `$oc_path/bin/ToolSetup.pl vcsmx`
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
