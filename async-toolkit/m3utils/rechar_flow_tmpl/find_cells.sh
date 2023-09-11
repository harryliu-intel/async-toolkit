#!/bin/sh

# usage : find_cells <cell_file>

libroot=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v0p0_efv
libpvt0=lib783_i0s_160h_50pp_
libpvt1=_100c_tttt_cmax

here=`pwd`
af=${here}/allcells.dat
sf=${here}/soughtcells.dat
cf=$1

rm -f $af

pushd ${libroot} 2>&1 > /dev/null

for bundle in *_*vt; do
    spfdir=${bundle}/spf/${libpvt0}${bundle}${libpvt1}
    if [ -d $spfdir ]; then
        pushd ${spfdir} 2>&1 > /dev/null
        ls *.spf | sed -e 's/.spf$//' -e "s/^/${bundle} /"  >> $af
        popd 2>&1 > /dev/null
    fi
done

popd 2>&1 > /dev/null
sort -k 2 ${af} > ${af}$$
mv ${af}$$ ${af}

sort $cf > $sf

join -1 2 -2 1 allcells.dat soughtcells.dat | awk '{print $2 " " $1}'



