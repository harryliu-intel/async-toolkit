# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/cad/oldC/main/solve/progfiles-custom.mk#3 $
# $DateTime: 2003/10/25 04:40:31 $
# $Author: lines $

default: \
    $(CURR_TARGET_DIR)/rdgds \
    $(CURR_TARGET_DIR)/wrgds \
    $(CURR_TARGET_DIR)/aaggds 

MAKE_LINK_RULE_TARGET_NAME := rdgds
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/rdgds.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

MAKE_LINK_RULE_TARGET_NAME := wrgds
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/wrgds.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
MAKE_LINK_RULE_TARGET_NAME := aaggds
#PROGFILES_PROJECT_LIBS += readline termcap
PROGFILES_PROJECT_LIBS += readline
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/aaggds.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
