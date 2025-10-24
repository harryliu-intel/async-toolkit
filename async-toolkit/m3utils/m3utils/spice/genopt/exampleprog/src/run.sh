#!/bin/sh -x
../AMD64_LINUX/exampleprog -vdd 0.1 -delp 0.1 -deln 0.1
${M3UTILS}/spice/schemagraph/schemaeval/AMD64_LINUX/schemaeval -schema ./schema.dat -data example.out -eval 'result'

