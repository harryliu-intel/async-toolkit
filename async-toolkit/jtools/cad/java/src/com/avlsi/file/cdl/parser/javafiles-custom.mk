CURR_RESULTS_FILES := $(CURR_RESULT_FILES) $(CURR_TARGET_DIR)/RenumberFactory.sh

$(CURR_TARGET_DIR)/RenumberFactory.sh: \
	$(CURR_PROJECT_DIR)/../../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.file.cdl.parser.RenumberFactory/" >$@
