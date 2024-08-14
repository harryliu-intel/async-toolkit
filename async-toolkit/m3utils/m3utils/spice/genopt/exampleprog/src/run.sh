#!/bin/sh -x
../AMD64_LINUX/exampleprog -vdd 0.1 -delp 0.1 -deln 0.1
~/work/m3utils/spice/schemagraph/schemaeval/AMD64_LINUX/schemaeval -schema ./schema.dat -data example.out -eval 'result'

