#!/bin/sh -x

rm -rf wm_meta_common mby_struct
mkdir -p wm_meta_common/src
mkdir -p mby_struct/src

../../structgen/AMD64_LINUX/structgen interlang.scm exit

cd wm_meta_common
cm3 -x
cd -
cd mby_struct
cm3 -x
