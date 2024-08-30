#!/bin/sh

path=$1
shift

~/work/m3utils/spice/schemagraph/schemaeval/AMD64_LINUX/schemaeval -schema ~/work/m3utils/spice/ringosc/lowtemp/src/schema.dat -scm ~/work/m3utils/spice/ringosc/lowtemp/src/defs.scm -data $path -eval "$*"
