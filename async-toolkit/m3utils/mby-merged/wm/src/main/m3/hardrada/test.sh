#!/usr/bin/sh

rm -rf ./Lolz
rm -rf ./depgraph_tree.m3
rm -rf ./main_parse_root
rm -rf ./depgraph_pretest.m3
rm -rf ./depgraph_pretest_next.m3
rm -rf ./depgraph_pretest_afterplaceholder.m3
rm -rf ./depgraph_pretest_b4placeholder.m3
rm -rf ./w_placeholder
rm -rf ./w_placeholder_if1
rm -rf ./w_placeholder_if2
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
