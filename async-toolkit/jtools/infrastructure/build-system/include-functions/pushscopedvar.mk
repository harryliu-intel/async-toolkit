# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


$(strip $(PUSH_SCOPED_VAR_VAR_NAME))_FRAME_SIZE          := $(words $($(strip $(PUSH_SCOPED_VAR_VAR_NAME))_TEMP) ADD_PLACE_HOLDER)
$(strip $(PUSH_SCOPED_VAR_VAR_NAME))_FRAME_SIZE_STACK    := $($(strip $(PUSH_SCOPED_VAR_VAR_NAME))_FRAME_SIZE) \
                                                   $($(strip $(PUSH_SCOPED_VAR_VAR_NAME))_FRAME_SIZE_STACK)
$(strip $(PUSH_SCOPED_VAR_VAR_NAME))_STACK               := $($(strip $(PUSH_SCOPED_VAR_VAR_NAME))_TEMP) ADD_PLACE_HOLDER $($(strip $(PUSH_SCOPED_VAR_VAR_NAME))_STACK)
