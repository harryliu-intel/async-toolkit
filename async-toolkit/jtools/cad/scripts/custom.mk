# Copyright 2005 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

$(CURR_TARGET_DIR)/inliner.sh: \
	$(CURR_PROJECT_DIR)/../java/scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.file.cdl.parser.Inliner/"  >$@
