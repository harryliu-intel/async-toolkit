# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

PROGFILES_PROJECT_LIB_DIRS := $(PROGFILES_PROJECT_LIB_DIRS) $(CURR_TARGET_DIR)
ifeq ($(MAKEFILE_KERNEL_NAME),Linux)
  PROGFILES_PROJECT_LIB_DIRS := $(PROGFILES_PROJECT_LIB_DIRS)
endif

default: $(CURR_TARGET_DIR)/aspice        \
         $(CURR_TARGET_DIR)/test_model     \
         $(CURR_TARGET_DIR)/aplot         \
         $(CURR_TARGET_DIR)/reorder_trace \
         $(CURR_TARGET_DIR)/convert_trace \
         $(CURR_TARGET_DIR)/debug_trace \
	 $(CURR_TARGET_DIR)/rc_simplify

MAKE_LINK_RULE_TARGET_NAME := aspice
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
                                      $(CURR_TARGET_DIR)/aspice.o                                   \
                                      $(CURR_TARGET_DIR)/libaspice.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk


MAKE_LINK_RULE_TARGET_NAME := test_model
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
                                      $(CURR_TARGET_DIR)/test_model.o                                \
                                      $(CURR_TARGET_DIR)/libaspice.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk


MAKE_LINK_RULE_TARGET_NAME := aplot
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
                                      $(CURR_TARGET_DIR)/aplot.o                                    \
                                      $(CURR_TARGET_DIR)/tracelib.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

MAKE_LINK_RULE_TARGET_NAME := reorder_trace
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
                                      $(CURR_TARGET_DIR)/reorder_trace.o                            \
                                      $(CURR_TARGET_DIR)/libaspice.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

MAKE_LINK_RULE_TARGET_NAME := convert_trace
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
                                      $(CURR_TARGET_DIR)/convert_trace.o                            \
                                      $(CURR_TARGET_DIR)/libaspice.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

MAKE_LINK_RULE_TARGET_NAME := debug_trace
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
                                      $(CURR_TARGET_DIR)/debug_trace.o                              \
                                      $(CURR_TARGET_DIR)/libaspice.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

MAKE_LINK_RULE_TARGET_NAME := rc_simplify
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../lib/liblib.o) \
                                      $(CURR_TARGET_DIR)/rc_simplify.o

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
