#!/bin/sh -x

time ../AMD64_LINUX/spiceflat tcam.sp ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl
sed 's/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_//' gprof.out > s_gprof.out
sed 's/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_//' bflat.out > s_bflat.out
