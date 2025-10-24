#!/usr/intel/bin/zsh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# usage:
# merge_releases_ndm.sh <target area> <release area 0> <release area 1> ...
# run from top level of a rechar checkout

tgtdir=$1
shift
areas=("$@")

TOP=${0:h}
cell_list="cell_list"
source ${TOP}/env.zsh

if [[ -d $tgtdir ]]; then
    echo "error : $tgtdir exists"
    exit 1
fi

mkdir $tgtdir
tgtdir=`realpath $tgtdir`
mkdir $tgtdir/ndms

ai=0
for area in $areas; do
    pushd $area/bundles || exit 1
    ls >> $tgtdir/bundles.txt

    for bundle in *_*vt; do
        pushd $bundle/lib
        for ldb in *.ldb; do
            if [[ ! -L $ldb ]]; then
                echo "not symlink: $ldb ; $ai"
                tgt=${ldb:r}_${ai}.ldb
                cp $ldb $tgtdir/ndms/$tgt
            fi
        done
        popd
    done
    popd
    (( ai = $ai + 1 ))
done

bundles=(`sort $tgtdir/bundles.txt | uniq`)

cd $tgtdir/ndms
$TOP/scripts/gen_ndms $bundles

mkdir $tgtdir/bundles
cd $tgtdir/bundles
$TOP/scripts/rebundle $tgtdir/ndms $bundles

echo "**********  $0 done at `date`"


