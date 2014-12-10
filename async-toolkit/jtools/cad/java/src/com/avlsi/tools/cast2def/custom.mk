CURR_RESULT_FILES := $(CURR_TARGET_DIR)/cast2def.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/cast2def.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.cast2def.Cast2Def/"  >$@
