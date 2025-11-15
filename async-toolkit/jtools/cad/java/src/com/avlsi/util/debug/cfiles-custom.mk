# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

CFILES_PROJECT_INCLUDE_DIRS := $(CURR_TARGET_DIR)/ $(CURR_PROJECT_DIR)/ $(JAVAFILES_JDK_ROOT)/include/


ifeq ("$(UNAME)","SunOS")
CFILES_PROJECT_INCLUDE_DIRS := $(CFILES_PROJECT_INCLUDE_DIRS) $(JAVAFILES_JDK_ROOT)/include/solaris
else

ifeq ("$(UNAME)","Linux")
CFILES_PROJECT_INCLUDE_DIRS := $(CFILES_PROJECT_INCLUDE_DIRS) $(JAVAFILES_JDK_ROOT)/include/linux
endif

endif
