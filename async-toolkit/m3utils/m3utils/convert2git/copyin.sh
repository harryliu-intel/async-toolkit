#!/bin/sh -x
WRKDIR=reduced_cvs
SRC=/home/gcap-cvs
TGT=/usr/home/mika/to_intel/${WRKDIR}

GIT=meta
BARE=${GIT}.git


rm -rf data 
rm -rf $BARE
rm -rf $GIT

rm -rf ${WRKDIR}
mkdir ${WRKDIR}

cat intel-p4.filelist | grep -v /meta/ | grep -v /wrap/ | grep -v /uglyprint/ | grep -v /nb/ | grep -v /meta/src/ | grep -v /timing/ | grep -v /schedule/ | grep -v /schedule2/ | grep -v /ecc/ | grep -v /stod/ | grep -v /metalib/ | grep -v /pg/ | grep -v /spice/ | grep -v /tcam/ | grep -v '/CVS/' | grep -v 'Entries$' | grep -v 'Repository$' | grep -v 'Root$' | grep -v FreeBSD4 | sed -e 's/$/,v/' | (cd ${SRC}; cpio -p -d -R mika ${TGT})

cp -r ${SRC}/CVSROOT ${TGT}

mkdir data 
cvs2git --blobfile=data/git-blob.dat \
        --dumpfile=data/git-dump.dat \
        --username=cvs2git \
        ./reduced_cvs

# now we have the git blobs in the data dir

mkdir $BARE
cd $BARE
git init --bare

git fast-import --export-marks=../data/git-marks.dat < ../data/git-blob.dat
git fast-import --import-marks=../data/git-marks.dat < ../data/git-dump.dat
git gc

cd ..

git clone $BARE
