#!/usr/intel/bin/zsh 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0



# for...

volt_steps=(100 200 300)

blah=(a b c)

for w in $blah; do
    echo $w
done

for v in $volt_steps; do
    echo "got $v"
done

exit 0

# fiddle with arrays in zsh

a=$1
shift
b=$1
shift


stuff=("$@")

echo "a     is $a"
echo "b     is $b"
echo "stuff is $stuff"

for s in $stuff; do
    echo "s is $s"
done
