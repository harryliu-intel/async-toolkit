# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# Copyright 2005 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

$(CURR_TARGET_DIR)/inliner.sh: \
	$(CURR_PROJECT_DIR)/../java/scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.file.cdl.parser.Inliner/"  >$@
