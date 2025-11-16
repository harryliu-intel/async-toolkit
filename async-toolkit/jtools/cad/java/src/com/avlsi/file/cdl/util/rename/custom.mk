# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


CURR_RESULT_FILES := $(CURR_RESULT_FILES)                              \
                     $(CURR_TARGET_DIR)/cdl_renamer.sh                 \
                     $(CURR_TARGET_DIR)/rename.sh

$(CURR_TARGET_DIR)/cdl_renamer.sh: \
	$(CURR_PROJECT_DIR)/../../../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.file.cdl.util.rename.CDLRenamer/" >$@

$(CURR_TARGET_DIR)/rename.sh: \
	$(CURR_PROJECT_DIR)/../../../../../../../scripts/fulcrum-rename.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.file.cdl.util.rename.Rename/" >$@
