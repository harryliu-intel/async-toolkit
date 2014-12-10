# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/main/cad/c/auto/progfiles-custom.mk#1 $
# $DateTime: 2005/11/07 14:51:55 $
# $Author: aubrey $

default: $(CURR_TARGET_DIR)/bigint_calculator

#PROGFILES_PROJECT_LIBS += readline termcap
PROGFILES_PROJECT_LIBS += readline
MAKE_LINK_RULE_TARGET_NAME := calculator
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o)     \
                                      $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/expression.o) \
                                      $(CURR_TARGET_DIR)/calculator.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
MAKE_LINK_RULE_TARGET_NAME := bigint_calculator
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o)     \
                                      $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/expression.o) \
                                      $(CURR_TARGET_DIR)/bigint_calculator.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
