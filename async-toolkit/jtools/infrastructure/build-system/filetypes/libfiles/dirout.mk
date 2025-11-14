# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0



ifeq ($(LIBFILES_VARS_CHANGED),1)

POP_SCOPED_VAR_VAR_NAME := LIBFILES_PROJECT_LINK_FLAGS
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := LIBFILES_LINKER
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk


endif


LIBFILES_VARS_CHANGED :=$(strip $(firstword $(LIBFILES_VARS_CHANGED_STACK)))
LIBFILES_VARS_CHANGED_STACK := $(call POP_STACK,$(LIBFILES_VARS_CHANGED_STACK))

