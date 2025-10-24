#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.
# SPDX-License-Identifier: Apache-2.0


# run at the directory holding the repo

TGTNAME=$1
CLONE="${TGTNAME}-1"

git clone --no-local ${TGTNAME} ${CLONE} > ${CLONE}.clone.out 2>&1

touch paths.txt
paths=`realpath paths.txt`

echo "paths : " ${paths}

cd ${CLONE}

remove="git filter-repo --sensitive-data-removal --invert-paths"

# remove various GPL things


cat << EOF > ${paths}
async-toolkit/mby-merged/wm/src/main/m3/hardrada/hda/Similix-master
async-toolkit/mby-merged/wm/src/main/m3/hardrada/hda/scheme48
async-toolkit/mby-merged/wm/src/main/m3/hardrada/hda/scm
async-toolkit/mby-merged/wm/src/main/m3/hardrada/hda/slib
EOF

# remove any actual RTL sources

find . -type d -and -name rtl | grep mby-merged | grep src | sed 's,^\./,,' >> ${paths}

find async-toolkit/rechar_flow_tmpl -type f -and -name cell_list.\*  >> ${paths}

${remove} --paths-from-file ${paths}


