# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

CPPFILES_COMPILER     := $(GCXX)
CPPFILES_PREPROCESSOR := $(CPPFILES_COMPILER) -E
CPPFILES_CPP_COMPILE_FLAGS := -fpic
