# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

CURR_RESULT_FILES := $(CURR_TARGET_DIR)/cast_file_server.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/cast_file_server.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.cast2.util.CastFileServer/"  >$@
