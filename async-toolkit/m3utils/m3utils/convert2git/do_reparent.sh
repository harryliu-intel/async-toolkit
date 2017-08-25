#!/bin/sh -x

# setup
mkdir merge
cd merge

# grab the P4-git repo from Intel
git clone git25@gf25.devtools.intel.com:hlp-hw

# grab the CVS-git repo from Caltech/Mika
git clone ../work/external_meta xmeta

#cp -r merge.save merge

# work in the Caltech repo
cd xmeta

# update the email addresses

fix_name()
{
export OLD_NAME=$1
export NEW_NAME=$2
export NEW_EMAIL=$3

echo OLD_NAME ${OLD_NAME}
echo NEW_NAME ${NEW_NAME}
echo NEW_EMAIL ${NEW_EMAIL}

git filter-branch -f --env-filter '
echo $GIT_AUTHOR_NAME

if [ "$GIT_COMMITTER_NAME" = "$OLD_NAME" ]
then
    echo committer match
    export GIT_COMMITTER_NAME="$NEW_NAME"
    export GIT_COMMITTER_EMAIL="$NEW_EMAIL"
fi
if [ "$GIT_AUTHOR_NAME" = "$OLD_NAME" ]
then
    echo author match
    export GIT_AUTHOR_NAME="$NEW_NAME"
    export GIT_AUTHOR_EMAIL="$NEW_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags
}

fix_name "mika"     "Mika Nystrom"           "mika@alum.mit.edu"
fix_name "kp"       "Karl S. Papadantonakis" "kp@async.caltech.edu"
fix_name "abe"      "Abraham Ankumah"        "abe@async.caltech.edu"
fix_name "elaine"   "Elaine Ou"              "elaine@async.caltech.edu"
fix_name "flyingpi" "Wonjin Jang"            "flyingpi@async.caltech.edu"
fix_name "gareth"   "Gareth White"           "gareth.white@gmail.com"
fix_name "jd"       "Jon Dama"               "jd@async.caltech.edu"
fix_name "penzes"   "Paul I. Penzes"         "penzes@async.caltech.edu"
fix_name "piyush"   "Piyush Prakash"         "piyush@async.caltech.edu"
fix_name "thielman" "Mike Thielman"          "thielman@async.caltech.edu"
fix_name "tom"      "Tom Hutchinson"         "thomashutchinson@hotmail.com"
fix_name "wongca"   "Catherine G. Wong"      "wongca@async.caltech.edu"

git update-ref -d refs/original/refs/heads/master

cd ..

# work in the Intel repo
cd hlp-hw

# strip out the two top-level dirs of the Intel repo (brings us down to tools/meta)
git filter-branch --prune-empty --subdirectory-filter tools/meta master

# add Caltech repo as remote and fetch it
git remote add xmeta ../xmeta
git fetch xmeta
git branch xmeta-master xmeta/master

INTEL_EPOCH=`git log master | grep Date | tail -1`

echo "The Epoch is ${INTEL_EPOCH}"

CALTECH_FINAL=`git log xmeta/master --before "${INTEL_EPOCH}" | grep commit | head -1 | awk '{print $2}'`

echo "Last foreign hash of interest is ${CALTECH_FINAL}"

# this magic is the final commit in the Caltech repo dated before the 
# first commit at Intel (got it from scanning the commit dates)
prev=${CALTECH_FINAL}

# set up prev pointer
git co -b prev ${prev}

# the list of commits to reparent -- in REVERSE order
commits=`git log master | grep commit | awk '{print $2}' | tac`

for c in ${commits}; do
	git branch -D cur
	git co -b cur ${c}
	git reparent -p prev
  	# note that git reparent changes the SHA of the node that is reparented
	# so to keep the chain intact, we need to request the hash id of cur
	n=`git log | grep commit | head -1 | awk '{print $2}'`
	git co master
	git branch -D prev
	git co -b prev ${n}
done

# clean up.
# at this point, master is our old repo
# cur points to the reparented version of master
# delete master, make a new master
git co cur
git branch -D master
git co -b master cur

# clean up
git branch -D cur
git branch -D prev
git branch -D xmeta-master

git gc

