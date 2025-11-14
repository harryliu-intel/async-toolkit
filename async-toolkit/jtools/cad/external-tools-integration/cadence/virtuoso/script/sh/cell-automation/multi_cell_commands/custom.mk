# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(CURR_TARGET_DIR)/updatenetlist.sh


$(CURR_TARGET_DIR)/updatenetlist.sh: $(CURR_PROJECT_DIR)/updatenetlist.sh
	cat $< | $(GNUSED) -e "s/\\\$$buildid\\\$$/$(FULCRUM_BUILD_ID)/" >$@
