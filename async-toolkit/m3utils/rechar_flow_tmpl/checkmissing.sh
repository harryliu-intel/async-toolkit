#!/bin/sh


# catalog missing cells in a run

ls -l siliconsmart/bundles/*/lib/*.lib.gz | grep -v ^l | awk '{print $9}' | xargs -n1 zgrep 'cell(i0s' | sed 's/^.*cell(\(i0s[a-z0-9]*\)).*$/\1/' | sort | uniq > generated.cells
awk '{print $2}' cell_list | sort | uniq > required.cells

cat required.cells required.cells generated.cells | sort | uniq -c | grep -v " 3 "

