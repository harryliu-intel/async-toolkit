# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

$(CURR_TARGET_DIR)/generate_ip_data.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.ipgen.GenerateIPData/"  >$@
