#!/usr/bin/awk -f

# print any number in scientific notation in a lib file

/cell.i0s/ { cell = $0; gotcell=1 }

/e\+2/ { if (gotcell) { print cell ; print $0; gotcell=0 } }

