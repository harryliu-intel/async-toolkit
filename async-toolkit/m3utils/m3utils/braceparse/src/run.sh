#!/bin/sh -x

zcat example.net.gz | ../AMD64_LINUX/netlist2goxrpt -f - -W n7 -t transistor.cells 
