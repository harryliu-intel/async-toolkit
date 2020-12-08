#!/bin/sh -x

zcat example2.net.gz | ../AMD64_LINUX/netlist2goxrpt -f - -r test -T gox -l 3
