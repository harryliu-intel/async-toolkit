
CURR_RESULT_FILES := $(CURR_TARGET_DIR)/csp2xml.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/csp2xml.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.csp.csp2xml.Csp2Xml/"  >$@
