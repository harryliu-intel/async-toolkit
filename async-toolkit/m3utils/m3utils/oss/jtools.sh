#!/bin/sh -x

# SCHEMATIC setup of jtools (was done manually)
#
exit 1  # do NOT run this


COPYRIGHT=/nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/oss/async-toolkit/async-toolkit/m3utils/m3utils/oss/copyright.sh 
WORKDIR=/nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/oss/jtools-work


# go to work area
cd ${WORKDIR}

# clone jtools repo
git clone --no-local jtools-orig jtools-1

cd jtools-1

# change paths to be compatible with main repo
git filter-repo --to-subdirectory-filter async-toolkit/jtools

# add copyright statements
${COPYRIGHT} | & grep ERROR | awk '{print $6}' | awk -F. '{print $NF}' | sort | uniq -c | sort -g

# clone the main repo
cd ${WORKDIR}
git clone async-toolkit async-toolkit-jtools-merge
cd async-toolkit-jtools-merge

git branch jtools-merge
git checkout jtools-merge

git remote add jtools /nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/oss/jtools-work/jtools-1

git fetch jtools --tags
git merge --no-edit --allow-unrelated-histories jtools/master

git remote remove jtools
git remote remove origin

git remote add origin https://github.com/IntelLabs/async-toolkit.git

git config --global http.postBuffer 524288000
git push --set-upstream origin jtools-merge
