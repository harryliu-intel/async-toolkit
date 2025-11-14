# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#Recurse into subdirectories.



ifeq ($(CURR_SUB_TARGET_MK_INCLUDES),)
else
-include $(CURR_SUB_TARGET_MK_INCLUDES)
endif

