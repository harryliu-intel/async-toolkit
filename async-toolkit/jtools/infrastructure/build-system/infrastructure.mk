#
# Copyright 2001 Asynchronous Digital Design.  All rights reserved.
#
# Author: Jesse Rosenstock
#
# $Id$
#

#
# Infrastructure for build system.  Makes subdirectory makefiles, and
# possibly cvsignores in the future.
#


#         Inputs:
#           BUILD_SYSTEM_ROOT           The location of the root of the build system.
#           CURR_PROJECT_DIR            The current project directory we are building from.
#           CURR_TARGET_DIR             The current target directory we are building into.
#         Outputs:
#           None
#         Side Effects:
#           1.  Makefiles the rules in this file generate are added to the value of
#               CURR_INTERMEDIATE_MAKE_FILES


ifeq ("$(FULCRUM_NO_UTIL_MAKEFILES)","1")
makefiles: $(CURR_TARGET_DIR)/Makefile
else
makefiles: $(CURR_TARGET_DIR)/Makefile $(CURR_PROJECT_DIR)/Makefile
endif

default: makefiles

CURR_INTERMEDIATE_MAKE_FILES := $(CURR_TARGET_DIR)/Makefile $(CURR_PROJECT_DIR)/Makefile \
                                $(CURR_INTERMEDIATE_MAKE_FILES)

ifeq ("$(FULCRUM_NO_UTIL_MAKEFILES)","")
FULCRUM_NO_UTIL_MAKEFILES := 0
endif

#don't spam the user
$(CURR_TARGET_DIR)/Makefile: $(BUILD_SYSTEM_ROOT)/subdir.mk.gen
	@$(BUILD_SYSTEM_ROOT)/subdir.mk.gen "$(BUILD_SYSTEM_ROOT)"         \
                                           "$(ROOT_PROJECT_DIR)"          \
                                           "$(ROOT_TARGET_DIR)"           \
				           "$(FULCRUM_NO_UTIL_MAKEFILES)" \
                                           `dirname "$@"`                 \
					   "$(PWD)"                       \
				           "$(MAKE_WITH_MAKE_PWD_ONLY)"

$(CURR_PROJECT_DIR)/Makefile: $(CURR_TARGET_DIR)/Makefile
	cp "$<" "$@"
