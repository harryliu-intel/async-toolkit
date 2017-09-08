#!/bin/sh -x

for file in /nfs/sc/disks/mst_104/mwrighto/mst-mst-a0/results/regflow/fxp/fxp_osxml/ControlRegisters/*; do
	time ../AMD64_LINUX/testxmlparse $file 
done

