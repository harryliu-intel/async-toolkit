# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0



$(strip $(POP_SCOPED_VAR_VAR_NAME))_FRAME_SIZE        :=$(strip $(firstword $($(strip $(POP_SCOPED_VAR_VAR_NAME))_FRAME_SIZE_STACK)))
$(strip $(POP_SCOPED_VAR_VAR_NAME))_FRAME_SIZE_STACK  := $(call POP_STACK,$($(strip $(POP_SCOPED_VAR_VAR_NAME))_FRAME_SIZE_STACK))
$(strip $(POP_SCOPED_VAR_VAR_NAME))                   :=$(strip $(filter-out ADD_PLACE_HOLDER, \
                                                                 $(wordlist 1,$($(strip $(POP_SCOPED_VAR_VAR_NAME))_FRAME_SIZE),$($(strip $(POP_SCOPED_VAR_VAR_NAME))_STACK))))
$(strip $(POP_SCOPED_VAR_VAR_NAME))_STACK             := $(call STRIP_FIRST_N_WORDS,$($(strip $(POP_SCOPED_VAR_VAR_NAME))_FRAME_SIZE),\
                                                         $($(strip $(POP_SCOPED_VAR_VAR_NAME))_STACK))
