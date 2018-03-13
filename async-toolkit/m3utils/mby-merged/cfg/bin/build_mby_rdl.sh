#!/usr/intel/bin/tcsh -f


nebulon -fuse -ovm -html -out_dir . -input $MBY_ROOT/source/rdl/mby_fusegen_nebulon.rdl

cp  $MBY_ROOT/source/rdl/fusegen_api.vh $MBY_ROOT/verif/mby/tb/saola

cp -rf output/fuse/*.svh $MBY_ROOT/verif/mby/tb/saola

cp -rf output/html/* $MBY_ROOT/doc/rdl/

rm -rf output

rm -rf nebulon*
