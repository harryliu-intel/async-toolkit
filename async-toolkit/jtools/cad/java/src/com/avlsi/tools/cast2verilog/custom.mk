CURR_RESULT_FILES := $(CURR_TARGET_DIR)/cast2verilog.sh $(CURR_TARGET_DIR)/cast2rtl.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/cast2verilog.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.cast2verilog.Cast2Verilog/"  >$@

$(CURR_TARGET_DIR)/cast2rtl.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.cast2verilog.Cast2RTL/"  >$@
