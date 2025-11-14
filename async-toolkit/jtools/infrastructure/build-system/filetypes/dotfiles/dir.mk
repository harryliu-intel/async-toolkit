# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

CURR_TARGET_DOT_FILES := $(wildcard $(CURR_PROJECT_DIR)/*.dot )

ifneq ("$(strip $(CURR_TARGET_DOT_FILES))","")

CURR_INTERMEDIATE_FILES := $(patsubst $(CURR_PROJECT_DIR)/%.dot, $(CURR_TARGET_DIR)/%.dotoutput.ps, $(CURR_TARGET_DOT_FILES) ) $(CURR_INTERMEDIATE_FILES)
CURR_RESULT_FILES := $(patsubst $(CURR_PROJECT_DIR)/%.dot, $(CURR_TARGET_DIR)/%.eps, $(CURR_TARGET_DOT_FILES) ) $(CURR_RESULT_FILES)

#Implicit rule to convery .dot files to poscript files.  There is an impliciy rule to convert
#.dotoutput.ps files to .eps file in once.mk.
$(CURR_TARGET_DIR)/%.dotoutput.ps: $(CURR_PROJECT_DIR)/%.dot
	dot -Tps $< >$@

endif
