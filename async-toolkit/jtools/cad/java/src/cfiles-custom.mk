# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

CFILES_COMPILER     := $(GCC)
CFILES_PREPROCESSOR := $(CFILES_COMPILER) -E
CFILES_C_COMPILE_FLAGS := -fpic
ifeq ($(MAKEFILE_KERNEL_NAME),SunOS)
  CFILES_C_COMPILE_FLAGS += -m64
endif
