#!/bin/bash

# $1 technology
# $2 dfII/CAST project

# examples: 
# CASTPATH= glc tsmc13 x8 ~/cell_lists/eco1 --debug (pv1)
# CASTPATH= SPECPATH=/scratch/net2/nevada/ccar_support.v2 glc nv90 nv ~/cell_lists/nv90 --debug (nevada)

technology="$1"
project="$2"
shift 2

output_dir="$HOME/working/glc-$technology-$project"
mkdir -p "$output_dir"

${HWDIR:=$HOME/hw}
${DFIIDIR:=$HWDIR/layout/$technology/dfII/$project}

CASTPATH=$CASTPATH:$hw_dir/cast/$project:$hw_dir/cast/main:$SPECPATH:$hw_dir/layout/$technology/spec/$project:$hw_dir/layout/$technology/spec/main:$hw_dir/layout/$technology/spec

gen_leaf_cells \
--fulcrum-pdk-root=$FULCRUM_PDK_ROOT \
--dfII-dir=$DFIIDIR \
--cast-path=$CASTPATH \
--cell-list=$cell_list \
--output-root=$output_dir \
$@
