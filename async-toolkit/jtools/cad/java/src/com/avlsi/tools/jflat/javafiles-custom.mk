# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


$(CURR_TARGET_DIR)/JFlat.java_files_classbuild: $(call CONONICALIZE_PATH,$(CURR_TARGET_DIR)/../../csp/csp2java/runtime/javaclasses)

JAVAFILES_EXTRA_JAR_CLASSES := $(wildcard $(CURR_PROJECT_DIR)/../../util/cmdline/*.java) $(wildcard $(CURR_PROJECT_DIR)/../../csp/csp2java/runtime/*.java)
