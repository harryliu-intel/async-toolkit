#!/usr/intel/bin/tcsh -f


nebulon -fuse -ovm -html -out_dir . -input $TLM1_ROOT/source/rdl/tlm_fusegen_nebulon.rdl

cp  $TLM1_ROOT/source/rdl/fusegen_api.vh $TLM1_ROOT/verif/tb/saola

cp -rf output/fuse/*.svh $TLM1_ROOT/verif/tb/saola

cp -rf output/html/* $TLM1_ROOT/doc/rdl/

rm -rf output

rm -rf nebulon*
