#!/usr/intel/bin/tcsh -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0



nebulon -fuse -ovm -html -out_dir . -input $MBY_ROOT/source/rdl/mby_fusegen_nebulon.rdl

cp  $MBY_ROOT/source/rdl/fusegen_api.vh $MBY_ROOT/verif/mby/tb/saola

cp -rf output/fuse/*.svh $MBY_ROOT/verif/mby/tb/saola

cp -rf output/html/* $MBY_ROOT/doc/rdl/

rm -rf output

rm -rf nebulon*
