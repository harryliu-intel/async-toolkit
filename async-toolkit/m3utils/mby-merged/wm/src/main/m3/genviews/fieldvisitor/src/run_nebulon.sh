#!/bin/sh -x

ROOT=../../../../../../..

/p/hdk/rtl/cad/x86-64_linux30/dt/nebulon/d18ww24.4/bin/nebulon \
    -I "${ROOT}/tools/srdl ${ROOT}/tools/srdl/mby" \
    -input ${ROOT}/tools/srdl/mby/mby_ppe_parser_map.rdl \
    -timeout 60000 \
    -xml -crif -sverilog \
    -out_dir . \
    -keep
