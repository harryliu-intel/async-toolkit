$(CURR_TARGET_DIR)/generate_gdsII_data.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.layout.gdsII.GenerateGDSIIData/"  >$@
