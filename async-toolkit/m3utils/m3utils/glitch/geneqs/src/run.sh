#!/bin/sh -x

../AMD64_LINUX/geneqs -a mau_glue_afifo_rdata_logic_term_info_with_flops.txt -f  mau_glue_afifo_to_rdata_logic.txt 

for file in *.glitch; do
	echo $file

	../../AMD64_LINUX/glitch < ${file}
done
