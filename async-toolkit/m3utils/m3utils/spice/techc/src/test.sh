#!/bin/sh

hsp_dir=/p/hdk/cad/pdk/pdk764_r0.4HP3_22ww20.1
hspice_model_root=${hsp_dir}/models/core/hspice/m17_6x_2ya_2yb_2yc_2yd_1ye_1ga_mim3x_1gb__bumpp

../AMD64_LINUX/techc -tech 1276.4 -tran ulvt -mode dyn -p setup -p simulate -p convert -simu xa -T ckt.sp -O HSPICE_MODEL p1276_4.hsp -O HSPICE_MODEL_ROOT ${hspice_model_root} -volt 0.85 -temp 25


#xa circuit.sp
#~/work/m3utils/spice/ct/AMD64_LINUX/ct -fsdb ~/work/m3utils/spice/fsdb/src/nanosimrd -threads 4 -wthreads 1 -R 50e-12 xa.fsdb xa
cat xa.names
