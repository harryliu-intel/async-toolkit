# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$


PROGFILES_LINKER := $(GCC)
ifeq ($(MAKEFILE_MACHINE_TYPE),x86_64)
    PROGFILES_PROJECT_LIB_DIRS := /usr/lib64
else
    PROGFILES_PROJECT_LIB_DIRS := /usr/lib
endif
PROGFILES_PROJECT_LIBS := readline
PROGFILES_PROJECT_LINK_FLAGS := -shared
ifeq ($(MAKEFILE_KERNEL_NAME),SunOS)
    PROGFILES_PROJECT_LINK_FLAGS += -m64
endif

MAKE_LINK_RULE_TARGET_NAME := libReadline.so
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(CURR_TARGET_DIR)/libreadline.o


include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mksolinkrule.mk
