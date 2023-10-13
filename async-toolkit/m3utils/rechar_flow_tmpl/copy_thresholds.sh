#!/bin/sh 

# usage : $0 <cell_list>

# ensure all ULVT cells have LVT counterparts & v.v.

if=$1

grep '_ulvt.*.a....x. *$' ${if} | sed -e 's/_ulvt/_lvt/' -e 's/\([0-9][a-z]\)a\([0-9][a-z][0-9][0-9]x[0-9]\)/\1b\2/' > ${if}.x1
grep '_lvt.*.b....x. *$'  ${if} | sed -e 's/_lvt/_ulvt/' -e 's/\([0-9][a-z]\)b\([0-9][a-z][0-9][0-9]x[0-9]\)/\1a\2/' > ${if}.x2

cat ${if} ${if}.x1 ${if}.x2 | awk '{printf("%s %s\n", $1, $2)}' | sort | uniq
