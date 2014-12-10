# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/main/cad/c/solve/progfiles-custom.mk#1 $
# $DateTime: 2005/11/07 14:51:55 $
# $Author: aubrey $


default: $(CURR_TARGET_DIR)/rescuedb.so

PROGFILES_PROJECT_LINK_FLAGS := -D_GNU_SOURCE -fPIC -ldl
MAKE_LINK_RULE_TARGET_NAME := rescuedb.so
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
	$(CURR_PROJECT_DIR)/rescuedb.c

PROGFILES_LINKER := $(GCC)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mksolinkrule.mk
