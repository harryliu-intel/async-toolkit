#!/bin/sh -x

# make sure you run make_filelist.sh before running this script

cat intel-p4.filelist intel-p4.filelist intel-find.filelist | grep -v AMD64 | grep -v bdd.old | grep -v ARM_LINUX | grep -v /old/ | grep -v '~$' | grep -v /gnuplots/ | grep -v /00 | grep -v 'bak\.../' | grep -v scc0 | grep -v /CVS/ | grep -v ecc/output | grep -v ecc/csrc | grep -v simv.daidir | grep -v sc_ion | sort | uniq -c | sort -g | grep -v /bak0 | grep -v vplot/src/xa | grep -v inter.vpd | grep -v 'fsdb$' | grep " *1 " | less
