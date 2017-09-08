#!/bin/sh -x
rm -f rdl.l

cat *.dat | sort | grep -v '^$' | uniq | awk '{printf("T_%-15s \"%s\"\n",toupper($1),$1)}' > rdl.l.2

cat rdl.l.[0-9] > rdl.l

rm -f rdl.t

cat *.dat | sort | grep -v '^$' | uniq | awk '{printf("%%const T_%s\n", toupper($1))}' > rdl.t.1

cat rdl.t.[0-9] > rdl.t

for file in *.dat; do
  base=`basename ${file} .dat`
  cat ${file} | awk '{printf("  %-23s T_%-15s\n", $1, toupper($1))}' > rdl.y.${base}
done
