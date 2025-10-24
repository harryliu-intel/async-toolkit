# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

MAKE_LINK_RULE_TARGET_NAME := strip_redundant_def
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
	$(CURR_TARGET_DIR)/strip_redundant_def.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
