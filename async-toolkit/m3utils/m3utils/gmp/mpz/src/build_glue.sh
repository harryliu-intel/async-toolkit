#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


echo "(" > mpz.scm

grep "^__GMP_DECLSPEC " /usr/include/gmp.h | grep -v '(\*' | grep -v extern | sed 's/).*$/)/' | sed 's/^__GMP_DECLSPEC //' | sed 's/\([a-z0-9_]*\) (/,\1(/' > decls.txt

cat decls.txt | sed 's/(/,/' | sed 's/)//' | sed 's/, /,/g' | sed 's/ ,/,/' | sed 's/ /-/g'  |  sed 's/,/ /g'  > commas.txt

cat commas.txt | awk '{printf("(%s %s", $2, $1); for (i = 3 ; i <= NF ; ++i) printf(" %s", $i); printf(")\n");}'  >> mpz.scm

echo ")" >> mpz.scm





