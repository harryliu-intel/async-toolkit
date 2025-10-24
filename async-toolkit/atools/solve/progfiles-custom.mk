# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

default: $(CURR_TARGET_DIR)/solve $(CURR_TARGET_DIR)/libsolve.so

MAKE_LINK_RULE_TARGET_NAME := solve
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/recalc.o) \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/expression.o) \
	$(CURR_TARGET_DIR)/solve.o \
	$(CURR_TARGET_DIR)/solvelib.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk


PROGFILES_PROJECXT_LINK_FLAGS := -shared
MAKE_LINK_RULE_TARGET_NAME := libsolve.so
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/recalc.o) \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/expression.o) \
	$(CURR_TARGET_DIR)/solvelib.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mksolinkrule.mk
