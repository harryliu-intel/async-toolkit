# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


$(CURR_TARGET_DIR)/Sigscan.h: $(CURR_TARGET_DIR)/Sigscan
	$(JAVAFILES_LOCAL_JNI_JAVAH-$(@D)) -o $@ com.avlsi.tools.sigscan.Sigscan


CURR_RESULT_FILES := $(CURR_TARGET_DIR)/Sigscan.h $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/sigscan.o: $(CURR_TARGET_DIR)/Sigscan.h


