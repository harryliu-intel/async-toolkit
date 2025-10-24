#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.
# SPDX-License-Identifier: Apache-2.0


ROOT=/nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/oss
ORIGDIR=${ROOT}/orig
PATHSDIR=${ROOT}/paths
MERGEDIR=${ROOT}/merged

HOME=`pwd`
MAIN=async-toolkit
#HOME=`pwd`
M3INSTALL=${ORIGDIR}/m3utils
MAIN=async-toolkit
TGTNAME=merged-git

# we point out copyright specially because the MAIN is probably a clean
# checkout, so it won't contain a built binary...

COPYRIGHT=/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/copyright/AMD64_LINUX/copyright

cd ${ORIGDIR}
repos=`echo *`
echo "REPOS : ${repos}"

rm -rf ${PATHSDIR}
mkdir ${PATHSDIR}

# put repos in correct subdir location
for r in ${repos}; do
    cd ${PATHSDIR}
    git clone --no-local ${ORIGDIR}/${r} > ${r}.clone.out 2>&1

    cd ${PATHSDIR}/${r}
    if [ "${r}" != "${MAIN}" ]; then
        git filter-repo --to-subdirectory-filter \
            ${MAIN}/${r}
    fi
done

# do the actual merge

echo "==================================  MERGING  =================================="

rm -rf ${MERGEDIR}
mkdir ${MERGEDIR}
cd ${MERGEDIR}

git clone ${PATHSDIR}/${MAIN} ${TGTNAME} > ${MAIN}.clone.out 2>&1

cd ${TGTNAME}

for r in ${repos}; do
    if [ "${r}" == "${MAIN}" ]; then
        echo "skipping ${r}"
    else
        echo "merging ${r}"
        git remote add ${r} ${PATHSDIR}/${r}
        git fetch ${r} --tags
        git merge --no-edit --allow-unrelated-histories ${r}/master > ${r}.master.merge.out 2>&1
        git merge --no-edit --allow-unrelated-histories ${r}/main > ${r}.main.merge.out 2>&1
    fi
done

cd ${MERGEDIR}

${M3INSTALL}/oss/censor.sh ${TGTNAME}

cd "${TGTNAME}-1"

${M3INSTALL}/oss/copyright.sh > copyright.out
