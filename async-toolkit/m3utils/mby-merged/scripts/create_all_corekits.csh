#!/bin/tcsh -f

cd $MODEL_ROOT/tools/collage/mpp
$MODEL_ROOT/scripts/create_corekit.csh build/builder.mpp.tcl
cd $MODEL_ROOT/tools/collage/epc
$MODEL_ROOT/scripts/create_corekit.csh build/builder.epc.tcl
cd $MODEL_ROOT/tools/collage/msh
$MODEL_ROOT/scripts/create_corekit.csh build/builder.msh.tcl
cd $MODEL_ROOT/tools/collage/gmm_s
$MODEL_ROOT/scripts/create_corekit.csh build/builder.gmm_s.tcl
cd $MODEL_ROOT/tools/collage/gmm_n
$MODEL_ROOT/scripts/create_corekit.csh build/builder.gmm_n.tcl

