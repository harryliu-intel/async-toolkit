#!/usr/intel/bin/zsh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# usage : find_cells.sh <cell_file>

source rechar_setup_env.zsh

for corner in $corners_list; do
    celsius=${corner:h:h:h:h:h}
    millivolts=${corner:h:h:h:h:t}
    trancorner=${corner:h:h:h:t}
    metalcorner=${corner:h:h:t}
    capcorner=${corner:h:t}
    metaltemp=${corner:t}
done

libroot=${stdcell_dir}
libpvt0=${techlib}_${stdlibname}_
libpvt1=_${metaltemp}c_${metalcorner}_${capcorner}

here=`pwd`
af=${here}/allcells.dat
sf=${here}/soughtcells.dat
rf=${here}/resultcells.dat
mf=${here}/missingcells.dat
cf=$1

rm -f $af || true

pushd ${libroot} 2>&1 > /dev/null

for bundle in *_*vt; do
    spfdir=${bundle}/spf/${libpvt0}${bundle}${libpvt1}

    echo "spfdir : $spfdir"
    
    if [ -d $spfdir ]; then
        pushd ${spfdir} 2>&1 > /dev/null
        ls *.spf | sed -e 's/.spf$//' -e "s/^/${bundle} /"  >> $af
        popd 2>&1 > /dev/null
    fi
done

popd 2>&1 > /dev/null
sort -k 2 ${af} > ${af}$$
mv ${af}$$ ${af}

sort $cf | sed -e 's/ //g' -e 's/\t//g' | uniq > $sf

join -1 2 -2 1 ${af} ${sf} | awk '{print $2 " " $1}' > ${rf}

echo "output is ${rf}"

echo "missing cells:"


awk '{print $2}' ${rf} | uniq | cat - ${sf} | sort | uniq -c | grep -v " 2 " | awk '{print $2}' > ${mf}

cat ${mf}






