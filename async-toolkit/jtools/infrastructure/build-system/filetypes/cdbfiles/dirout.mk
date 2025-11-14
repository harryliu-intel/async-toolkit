# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

ifeq ($(MAKE_LAYOUT),1)

ifeq ($(CDBFILES_VARS_CHANGED),1)

POP_SCOPED_VAR_VAR_NAME := LVS_FLAGS
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := DRC_FLAGS
include $(BUILD)/include-functions/popscopedvar.mk

endif

CDBFILES_VARS_CHANGED :=$(strip $(firstword $(CDBFILES_VARS_CHANGED_STACK)))
CDBFILES_VARS_CHANGED_STACK := $(call POP_STACK,$(CDBFILES_VARS_CHANGED_STACK))

endif
