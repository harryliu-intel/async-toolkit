#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


../AMD64_LINUX/geneqs -a mau_glue_afifo_rdata_logic_term_info_with_flops.txt -f  mau_glue_afifo_to_rdata_logic.txt 

for file in *.glitch; do
	echo $file

	../../AMD64_LINUX/glitch < ${file}
done
