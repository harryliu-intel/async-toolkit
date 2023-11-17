#!/usr/intel/bin/zsh

cell_list="cell_list"
source env.zsh

# usage : find_cells <cell_file>

libroot=${stdcell_dir}
libpvt0=${techlib}_${stdlibname}_
libpvt1=_100c_tttt_cmax

here=`pwd`
af=${here}/allcells.dat
sf=${here}/soughtcells.dat
rf=${here}/resultcells.dat
mf=${here}/missingcells.dat
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

sort $cf | uniq > $sf

join -1 2 -2 1 ${af} ${sf} | awk '{print $2 " " $1}' > ${rf}

echo "output is ${rf}"

echo "missing cells:"


awk '{print $2}' ${rf} | uniq | cat - ${sf} | sort | uniq -c | grep -v " 2 " | awk '{print $2}' > ${mf}

cat ${mf}






