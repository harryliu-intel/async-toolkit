# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


%.dvi: %.tex %.aux
	$(BUILD)/runindir.bash $(LATEX) `dirname $<` `basename $<`

%.aux %.log: %.tex
	$(BUILD)/runindir.bash $(LATEX) `dirname $<` `basename $<`

%.aux %.log: %.tex
	$(BUILD)/runindir.bash $(LATEX) `dirname $<` `basename $<`

%.dvi: %.tex %.aux
	$(BUILD)/runindir.bash $(LATEX) `dirname $<` `basename $<`

%.ps: %.dvi
	$(BUILD)/runindir.bash $(DVIPS) `dirname $<` `basename $<` -o `basename $@`

%.preview.ps: %.ps
	mpage -2 -bLetter -dp  $< >$@



%.pdf: %.dvi
	$(BUILD)/runindir.bash $(DVIPDF) `dirname $<` `basename $<` `basename $@`

INTERMEDIATEFILETYPES := $(INTERMEDIATEFILETYPES) .dvi .aux .log


