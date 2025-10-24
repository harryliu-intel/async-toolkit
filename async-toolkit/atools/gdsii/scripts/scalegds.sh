#!/bin/bash
# $Id$ AAG
function usage {
    echo "Usage: scalegds [-f] infile.gds outfile.gds";
    exit 1
}

force=0
if [ "$1" = "-f" ]; then
    force=1
    shift
fi
if [ -z "$1" -o -z "$2" -o "$1" = "$2" ]; then
    if [ "$1" = "$2" -a ! -z "$1" ]; then
        echo "You may not overwrite the input file.";
        exit 1;
    fi
    usage
fi
if [ -f "$2" -a $force -eq 0 ]; then
    echo "Overwrite of file $2 not permitted.";
    exit 1;
fi
x="`rdgds $1 | head -1 | sed -e 's/ [0-9][0-9]*//'`"
if [ "$x" != "HEADER" ]; then
    echo "$1 is not a GDS file."
    exit 1;
fi
ext1=`basename $1 | sed -e 's/.*\.//'`;
ext2=`basename $2 | sed -e 's/.*\.//'`;
x="`rdgds $1 | grep ^UNITS`"
u1=`echo $x | awk '{print $2}'`
u2=`echo $x | awk '{print $3}'`
u1=`/usr/intel/bin/perl -e "printf '%.3e', $u1*10;"`
u2=`/usr/intel/bin/perl -e "printf '%.3e', $u2*10;"`
rdgds $1 | sed -e "s/^UNITS.*/UNITS $u1 $u2/" | wrgds > $2
x=`cmp -l $1 $2 | wc -l`
if [ $x != 16 ]; then
    echo "Conversion failed because the wrong number of bytes were changed ($x)."
    exit 1;
fi
exit 0
