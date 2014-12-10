# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/cast2skill/custom.mk#2 $
# $DateTime: 2003/02/18 08:19:59 $
# $Author: chrisb $
CURR_RESULT_FILES := $(CURR_TARGET_DIR)/cdl2cast.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/cdl2cast.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.cdl2cast.CDL2Cast/"  >$@
