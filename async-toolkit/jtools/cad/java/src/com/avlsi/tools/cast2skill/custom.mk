# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
CURR_RESULT_FILES := $(CURR_TARGET_DIR)/cast2skill.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/cast2skill.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.cast2skill.Cast2Skill/"  >$@
