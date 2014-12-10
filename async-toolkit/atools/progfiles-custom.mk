# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
ifeq ($(MAKEFILE_KERNEL_NAME),Linux)
  PROGFILES_LINKER := $(GCC)
endif
ifeq ($(MAKEFILE_KERNEL_NAME),SunOS)
  PROGFILES_LINKER := $(GCC) -m64
endif


