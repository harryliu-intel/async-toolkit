# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$


PROGFILES_LINKER := $(GCC)
PROGFILES_PROJECT_LIB_DIRS := /usr/lib
ifeq ($(MAKEFILE_MACHINE_TYPE),x86_64)
    PROGFILES_PROJECT_LIB_DIRS += /nfs/site/eda/tools/cadence/incisive/14.10.002/linux_2.6.16_x86-64/tools/lib/64bit
else
    PROGFILES_PROJECT_LIB_DIRS += /nfs/site/eda/tools/cadence/incisive/14.10.002/linux_2.6.16_x86-64/tools/lib
endif
PROGFILES_PROJECT_LIBS := stdc++ dwapi dcapi sdi2-sst2-gcc_4.1
PROGFILES_PROJECT_LINK_FLAGS := -shared
ifeq ($(MAKEFILE_KERNEL_NAME),SunOS)
    PROGFILES_PROJECT_LINK_FLAGS += -m64
endif

MAKE_LINK_RULE_TARGET_NAME := libSigscan.so
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(CURR_TARGET_DIR)/libsigscan.o
include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mksolinkrule.mk
