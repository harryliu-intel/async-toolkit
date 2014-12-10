$(CURR_TARGET_DIR)/DSimMain.java_files_classbuild $(CURR_TARGET_DIR)/DSim.java_files_classbuild: $(call CONONICALIZE_PATH,$(CURR_TARGET_DIR)/../../util/cmdline/javaclasses)

$(CURR_TARGET_DIR)/DSimMain.java_files_classbuild $(CURR_TARGET_DIR)/DSim.java_files_classbuild: $(call CONONICALIZE_PATH,$(CURR_TARGET_DIR)/../../csp/csp2java/runtime/javaclasses)

JAVAFILES_EXTRA_JAR_CLASSES := $(wildcard $(CURR_PROJECT_DIR)/../../util/cmdline/*.java) $(wildcard $(CURR_PROJECT_DIR)/../../csp/csp2java/runtime/*.java)


CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(CURR_TARGET_DIR)/jdsim.sh

ifeq ($(MAKEFILE_MACHINE_TYPE),x86_64)
$(CURR_TARGET_DIR)/jdsim.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.dsim.DSimMain/" -e "s/\\\$$libs_to_preload\\\$$/\/usr\/lib64\/libncurses.so/"  >$@
else
$(CURR_TARGET_DIR)/jdsim.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.dsim.DSimMain/" -e "s/\\\$$libs_to_preload\\\$$/\/usr\/lib\/libncurses.so/"  >$@
endif
