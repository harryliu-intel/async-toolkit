#!/bin/bash
CURRDIR=`pwd`
RUNDIR=$2
COMMAND=$1
shift 2
cd $RUNDIR
$COMMAND $*
cd $CURRDIR 
