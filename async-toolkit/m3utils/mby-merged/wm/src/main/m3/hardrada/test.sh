#!/usr/bin/sh

rm -rf ./AMD64_LINUX
rm -rf ./test
rm -rf ./test_inline
rm -rf ./test_spec
rm -rf ./test_newspec
rm -rf ./test_out.m3
rm -rf ./test.m3
rm -rf ./depgraph.m3
/p/hdk/rtl/cad/x86-64_linux30/opensource/cm3/d5.10.0-20180711/bin/cm3
./AMD64_LINUX/test
diff ./test.m3 ./depgraph.m3
