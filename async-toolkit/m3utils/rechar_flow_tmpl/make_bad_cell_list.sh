#!/bin/sh

allcells=cell_list.all_releases
bad_bundle_file=bad_bundles
ofile=cell_list.bad_bundles

bad_bundles=`cat bad_bundles | tr \\012 \\040`

echo $bad_bundles

rm -f ${ofile}

for bundle in $bad_bundles; do

    grep "^${bundle}" ${allcells}  >> ${ofile}

done

