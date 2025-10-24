# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/cad/oldC/main/progfiles-custom.mk#4 $
# $DateTime: 2004/01/28 19:37:35 $
# $Author: lines $
ifeq ($(MAKEFILE_KERNEL_NAME),Linux)
  PROGFILES_LINKER := $(GCC)
endif
ifeq ($(MAKEFILE_KERNEL_NAME),SunOS)
  PROGFILES_LINKER := $(GCC) -m64
endif


