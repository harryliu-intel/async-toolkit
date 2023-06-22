#!/usr/bin/awk -f

BEGIN { FS="," }

{
    sumsq=0.0
    for (i=2; i <= NF; ++i)
        sumsq += $i*$i
    printf("%e %lf\n", $1, sqrt(sumsq)) 
}
