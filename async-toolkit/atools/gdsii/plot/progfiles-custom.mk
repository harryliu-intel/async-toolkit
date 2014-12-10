# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/cad/c/main/gdsii/dwg/progfiles-custom.mk#5 $
# $DateTime: 2004/10/02 13:57:30 $

default: \
	$(CURR_TARGET_DIR)/gds2plot

MAKE_LINK_RULE_TARGET_NAME := gds2plot
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/gds2plot.o) \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o) \
	$(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../../lib/liblib.o)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
