CURR_RESULT_FILES := $(CURR_TARGET_DIR)/cast_file_server.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/cast_file_server.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.cast2.util.CastFileServer/"  >$@
