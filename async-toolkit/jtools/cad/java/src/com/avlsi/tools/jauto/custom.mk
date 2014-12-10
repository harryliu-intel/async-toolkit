MAKE_ANTLR_RULES_G_FILE_NAME      := CellHierarchyDump.g
MAKE_ANTLR_RULES_LEXER_NAME       := CellHierarchyDumpLexer
MAKE_ANTLR_RULES_PARSER_NAME      := CellHierarchyDumpParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CellHierarchyDumpParser
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

MAKE_ANTLR_RULES_G_FILE_NAME      := CastQuery.g
MAKE_ANTLR_RULES_LEXER_NAME       := CastQueryLexer
MAKE_ANTLR_RULES_PARSER_NAME      := CastQueryParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CastQueryParser
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

CURR_RESULT_FILES := $(CURR_TARGET_DIR)/leafspec.sh $(CURR_TARGET_DIR)/jauto.sh $(CURR_TARGET_DIR)/cast2cdl.sh $(CURR_TARGET_DIR)/subtype_split.sh $(CURR_TARGET_DIR)/subtype_merge.sh $(CURR_TARGET_DIR)/merge_hint.sh $(CURR_TARGET_DIR)/caststat.sh $(CURR_TARGET_DIR)/cast_query.sh $(CURR_TARGET_DIR)/generate_charge_sharing_tests.sh $(CURR_TARGET_DIR)/count_prs.sh $(CURR_TARGET_DIR)/netlistDistance.sh $(CURR_TARGET_DIR)/generate_plt_subtypes.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/leafspec.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.LeafSpec/"  >$@

$(CURR_TARGET_DIR)/jauto.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.Jauto/"  >$@

$(CURR_TARGET_DIR)/cast2cdl.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.Cast2Cdl/"  >$@

$(CURR_TARGET_DIR)/subtype_split.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.SubtypeSplit/"  >$@

$(CURR_TARGET_DIR)/subtype_merge.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.SubtypeMerge/"  >$@

$(CURR_TARGET_DIR)/merge_hint.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.MergeHint/" -e 's/\$$PACKAGE_JRE_ARGS_DEFAULT\$$/-server/' >$@

$(CURR_TARGET_DIR)/caststat.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.CastStat/"  >$@

$(CURR_TARGET_DIR)/cast_query.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.CastQuery/"  >$@

$(CURR_TARGET_DIR)/generate_charge_sharing_tests.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.GenerateChargeSharingTests/"  >$@

$(CURR_TARGET_DIR)/count_prs.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.CountPrs/" >$@

$(CURR_TARGET_DIR)/netlistDistance.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.NetlistDistance/" >$@

$(CURR_TARGET_DIR)/generate_plt_subtypes.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.GeneratePLTSubtypes/" >$@

$(CURR_TARGET_DIR)/jtimer.sh: \
	$(CURR_PROJECT_DIR)/jtimer.sh
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.jauto.Jtimer/" >$@
