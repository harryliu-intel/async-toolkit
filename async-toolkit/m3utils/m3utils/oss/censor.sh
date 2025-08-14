#!/bin/sh -x

# run at the directory holding the repo

TGTNAME=$1
CLONE="${TGTNAME}-1"

git clone --no-local ${TGTNAME} ${CLONE} > ${CLONE}.clone.out 2>&1

paths=`pwd`/paths.txt

cd ${CLONE}

remove="git filter-repo --sensitive-data-removal --invert-paths"

# remove various GPL things


echo << EOF > ${paths}
m3utils/mby-merged/wm/src/main/m3/hardrada/hda/Similix-master
m3utils/mby-merged/wm/src/main/m3/hardrada/hda/scheme48
m3utils/mby-merged/wm/src/main/m3/hardrada/hda/scm
m3utils/mby-merged/wm/src/main/m3/hardrada/hda/slib
EOF

# remove any actual RTL sources

find . -type d -and -name rtl | grep mby-merged | grep src | sed 's,^\./,,' >> ${paths}


${remove} --paths-from-file ${paths}


