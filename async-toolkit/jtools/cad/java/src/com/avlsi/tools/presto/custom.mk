CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(CURR_TARGET_DIR)/presto.sh $(CURR_TARGET_DIR)/csp2tt.sh

$(CURR_TARGET_DIR)/presto.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.presto.PReSto/"  >$@

$(CURR_TARGET_DIR)/csp2tt.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.csp.csp2tt.Csp2TT/"  >$@
