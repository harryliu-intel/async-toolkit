# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$

# pngdwg eliminated on SunOS due to not having libpng available.

ifeq ($(MAKEFILE_KERNEL_NAME),SunOS)
default: \
    $(CURR_TARGET_DIR)/dwg \
    $(CURR_TARGET_DIR)/xdwg \
    $(CURR_TARGET_DIR)/gifdwg

else
ifeq ($(MAKEFILE_MACHINE_TYPE),x86_64)
default: \
    $(CURR_TARGET_DIR)/dwg \
    $(CURR_TARGET_DIR)/xdwg \
    $(CURR_TARGET_DIR)/gifdwg

else
default: \
    $(CURR_TARGET_DIR)/dwg \
    $(CURR_TARGET_DIR)/xdwg \
    $(CURR_TARGET_DIR)/gifdwg \
    $(CURR_TARGET_DIR)/pngdwg

endif
endif

MAKE_LINK_RULE_TARGET_NAME := dwg
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/dwg.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/post.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

MAKE_LINK_RULE_TARGET_NAME := gifdwg
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/dwg.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/gifdraw.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/gifmap.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../fonts/libfonts.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

ifneq ($(MAKEFILE_KERNEL_NAME), SunOS)
MAKE_LINK_RULE_TARGET_NAME := pngdwg
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/dwg.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/gifdraw.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/pngmap.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../fonts/libfonts.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o)
include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk
endif


PROGFILES_PROJECT_LIB_DIRS := /usr/X11R6/lib
ifeq ($(MAKEFILE_MACHINE_TYPE),x86_64)
   PROGFILES_PROJECT_LIB_DIRS := /usr/X11R6/lib64
endif
ifeq ($(MAKEFILE_KERNEL_NAME), SunOS)
PROGFILES_PROJECT_LIBS := Xaw Xmu Xt Xext X11 m
else
PROGFILES_PROJECT_LIBS := Xaw Xmu Xt Xext X11 png m
endif
MAKE_LINK_RULE_TARGET_NAME := xdwg
MAKE_LINK_RULE_TARGET_DEPENDENCIES := \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/dwg.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/xdraw.o) \
   $(call CONONICALIZE_PATH, $(CURR_TARGET_DIR)/../aag/libaag.o)

include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mklinkrule.mk

