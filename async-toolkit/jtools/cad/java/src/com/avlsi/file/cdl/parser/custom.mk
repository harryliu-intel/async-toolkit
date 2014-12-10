MAKE_ANTLR_RULES_G_FILE_NAME      := CDL.g
MAKE_ANTLR_RULES_LEXER_NAME       := CDLLexer
MAKE_ANTLR_RULES_PARSER_NAME      := CDLParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CDLLexer
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk


CURR_INTERMEDIATE_FILES := $(CURR_TARGET_DIR)/CDLWalker.java $(CURR_TARGET_DIR)/cdlstat.sh $(CURR_TARGET_DIR)/cdlaliases.sh $(CURR_INTERMEDIATE_FILES)

$(CURR_TARGET_DIR)/cdlstat.sh: \
	$(CURR_PROJECT_DIR)/../../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.file.cdl.parser.CDLstat/" >$@

$(CURR_TARGET_DIR)/cdlaliases.sh: \
	$(CURR_PROJECT_DIR)/../../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.file.cdl.parser.CDLAliases/" >$@
