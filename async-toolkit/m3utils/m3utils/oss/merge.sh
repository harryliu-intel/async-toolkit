#!/bin/sh -x

ROOT=/nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/oss
SRCDIR=${ROOT}/orig
PATHSDIR=${ROOT}/paths
MERGEDIR=${ROOT}/merged
HOME=`pwd`
MAIN=async-toolkit
TGTNAME=merged-git

cd ${SRCDIR}
repos=`echo *`
echo "REPOS : ${repos}"

rm -rf ${PATHSDIR}
mkdir ${PATHSDIR}

# put repos in correct subdir location
for r in ${repos}; do
    cd ${PATHSDIR}
    git clone --no-local ${SRCDIR}/${r} > ${r}.clone.out 2>&1

    cd ${PATHSDIR}/${r}
    git filter-repo --to-subdirectory-filter \
        ${MAIN}/${r}
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
        git merge --no-edit --allow-unrelated-histories ${r}/master > ${r}.merge.out 2>&1
    fi
done

cd ${MERGEDIR}

${ROOT}/orig/${MAIN}/oss/censor.sh ${TGTNAME}
