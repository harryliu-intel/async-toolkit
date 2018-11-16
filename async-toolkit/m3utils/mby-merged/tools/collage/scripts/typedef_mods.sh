#!/bin/bash
### This script is workaround for collage/coretool issue during corekit creation where it interprets typedef'd busses as only 1 bit wide
### https://hsdes.intel.com/resource/1909279599
### Will grab any typedef's busses from specified folder and substitute the values in the src module
# first arg is path to source rtl dir
# second arg is source rtl filename
# third arg is path to typedef files
cp $1/$2 ./$2
grep -r "^typedef.*logic.*\[" $3 | grep -v enum | awk -F"[ \t;]+" '{print $4 " " $2 " " $3}' > typelist.txt
while read -r line; do
  arr=($line)
  sed -i "s#${arr[0]}#${arr[1]} ${arr[2]}#g" ./$2
done < "typelist.txt"
rm typelist.txt
