# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


$(CURR_TARGET_DIR)/Readline.h: $(CURR_TARGET_DIR)/Readline
	$(JAVAFILES_LOCAL_JNI_JAVAH-$(@D)) -o $@ com.avlsi.util.readline.Readline


CURR_RESULT_FILES := $(CURR_TARGET_DIR)/Readline.h $(CURR_RESULT_FILES)


$(CURR_TARGET_DIR)/Modify.o      \
$(CURR_TARGET_DIR)/Bind.o        \
$(CURR_TARGET_DIR)/Undo.o        \
$(CURR_TARGET_DIR)/Readline.o     \
$(CURR_TARGET_DIR)/Redisplay.o   \
$(CURR_TARGET_DIR)/Keymap.o      \
$(CURR_TARGET_DIR)/Completion.o : $(CURR_TARGET_DIR)/Readline.h 
