# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

default: $(CURR_TARGET_DIR)/auto

MAKE_LINK_RULE_TARGET_NAME := auto
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o)     \
                                      $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/recalc.o)     \
                                      $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/expression.o) \
                                      $(CURR_TARGET_DIR)/auto.o                                         \
                                      $(CURR_TARGET_DIR)/libauto.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
