

CURR_RESULT_FILES := $(CURR_TARGET_DIR)/prs2verilog.sh $(CURR_TARGET_DIR)/generate_wrapper.sh $(CURR_TARGET_DIR)/generate_port_mapping.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/prs2verilog.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.prs2verilog.Prs2Verilog/"  >$@

$(CURR_TARGET_DIR)/generate_wrapper.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.prs2verilog.GenerateWrapper/"  >$@

$(CURR_TARGET_DIR)/generate_port_mapping.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.prs2verilog.GeneratePortMapping/"  >$@
