#!/bin/sh -x

time ../AMD64_LINUX/genpg -skipholes -bits 48 -sv full.sv -copyrightpath ${MODEL_ROOT}/scripts/intelcopyright.txt -crif crif.xml -G 4 MST_PG0 MST_PG1 MST_PG2 MST_PG3 -defpgnm DEFAULT_PG -basestrapbits some_pkg::BASE_STRAP_BITS --template hlp_pg_template.sv
