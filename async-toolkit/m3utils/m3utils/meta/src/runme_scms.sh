#!/bin/sh -x

#
# this code assumes that the .dsim and .routed files are already in
# /mnt/fulcrum/scratch1/mnystrom
# these files are themselves built by the scripts in ~mika/meta/meta:
#
# build.sh       master script, which calls:
#    dsim        builds PRS in "new-dsim" format
#    routed      builds list of routed cells
#


# run meta to make verification tasks

../LINUXLIBC6/meta -dir /mnt/fulcrum/scratch1/mnystrom -top $1 -dsim -routed << __META__

(load "make-verifications.scm")

(process-all-verifications)

__META__

# at this point, qsubbable scripts are sitting in ../meta

# submit scripts

cd ../scripts

../src/submit-all-scripts.sh

