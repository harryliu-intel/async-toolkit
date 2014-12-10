MAKE_ANTLR_RULES_G_FILE_NAME      := CastTwo.g
MAKE_ANTLR_RULES_LEXER_NAME       := CastTwoLexer
MAKE_ANTLR_RULES_PARSER_NAME      := CastTwoParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CastTwo
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk






cast_prs_results := $(CURR_TARGET_DIR)/CastPrsParser.java     \
                    $(CURR_TARGET_DIR)/CastPrsParserTokenTypes.java \
                    $(CURR_TARGET_DIR)/CastPrsParserTokenTypes.txt \
                    $(CURR_TARGET_DIR)/expandedCastPrs.g

$(CURR_TARGET_DIR)/CastPrs.g: $(CURR_PROJECT_DIR)/CastPrs.g
	cp $< $@
	chmod u+w $@

$(cast_prs_results): $(CURR_TARGET_DIR)/CastTwoParser.java $(CURR_TARGET_DIR)/CastPrs.g $(CURR_TARGET_DIR)/CastTwo.g
	cd $(@D) && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) -glib CastTwo.g CastPrs.g
	touch $@


CURR_INTERMEDIATE_FILES := $(cast_prs_results) $(CURR_TARGET_DIR)/CastPrs.g $(CURR_INTERMEDIATE_FILES)


cast_prs_results := $(CURR_TARGET_DIR)/CastSubtypesParser.java     \
                    $(CURR_TARGET_DIR)/CastSubtypesParserTokenTypes.java \
                    $(CURR_TARGET_DIR)/CastSubtypesParserTokenTypes.txt \
                    $(CURR_TARGET_DIR)/expandedCastSubtypes.g

$(CURR_TARGET_DIR)/CastSubtypes.g: $(CURR_PROJECT_DIR)/CastSubtypes.g
	cp $< $@
	chmod u+w $@

$(cast_prs_results): $(CURR_TARGET_DIR)/CastTwoParser.java $(CURR_TARGET_DIR)/CastSubtypes.g $(CURR_TARGET_DIR)/CastTwo.g
	cd $(@D) && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) -glib CastTwo.g CastSubtypes.g
	touch $@


CURR_INTERMEDIATE_FILES := $(cast_prs_results) $(CURR_TARGET_DIR)/CastSubtypes.g $(CURR_INTERMEDIATE_FILES)


cast_subcells_results := $(CURR_TARGET_DIR)/CastSubcellsParser.java     \
                    $(CURR_TARGET_DIR)/CastSubcellsParserTokenTypes.java \
                    $(CURR_TARGET_DIR)/CastSubcellsParserTokenTypes.txt \
                    $(CURR_TARGET_DIR)/expandedCastSubcells.g

$(CURR_TARGET_DIR)/CastSubcells.g: $(CURR_PROJECT_DIR)/CastSubcells.g
	cp $< $@
	chmod u+w $@

$(cast_subcells_results): $(CURR_TARGET_DIR)/CastTwoParser.java $(CURR_TARGET_DIR)/CastSubcells.g $(CURR_TARGET_DIR)/CastTwo.g
	cd $(@D) && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) -glib CastTwo.g CastSubcells.g
	touch $@


CURR_INTERMEDIATE_FILES := $(cast_subcells_results) $(CURR_TARGET_DIR)/CastSubcells.g $(CURR_INTERMEDIATE_FILES)


cast_aliases_results := $(CURR_TARGET_DIR)/CastAliasesParser.java     \
                    $(CURR_TARGET_DIR)/CastAliasesParserTokenTypes.java \
                    $(CURR_TARGET_DIR)/CastAliasesParserTokenTypes.txt \
                    $(CURR_TARGET_DIR)/expandedCastAliases.g

$(CURR_TARGET_DIR)/CastAliases.g: $(CURR_PROJECT_DIR)/CastAliases.g
	cp $< $@
	chmod u+w $@

$(cast_aliases_results): $(CURR_TARGET_DIR)/CastTwoParser.java $(CURR_TARGET_DIR)/CastAliases.g $(CURR_TARGET_DIR)/CastTwo.g
	cd $(@D) && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) -glib CastTwo.g CastAliases.g
	touch $@


CURR_INTERMEDIATE_FILES := $(cast_aliases_results) $(CURR_TARGET_DIR)/CastAliases.g $(CURR_INTERMEDIATE_FILES)


cast_assert_results := $(CURR_TARGET_DIR)/CastAssertParser.java     \
                    $(CURR_TARGET_DIR)/CastAssertParserTokenTypes.java \
                    $(CURR_TARGET_DIR)/CastAssertParserTokenTypes.txt \
                    $(CURR_TARGET_DIR)/expandedCastAssert.g

$(CURR_TARGET_DIR)/CastAssert.g: $(CURR_PROJECT_DIR)/CastAssert.g
	cp $< $@
	chmod u+w $@

$(cast_assert_results): $(CURR_TARGET_DIR)/CastTwoParser.java $(CURR_TARGET_DIR)/CastAssert.g $(CURR_TARGET_DIR)/CastPrs.g
	cd $(@D) && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) -glib "CastPrs.g;CastTwo.g" CastAssert.g
	touch $@


CURR_INTERMEDIATE_FILES := $(cast_assert_results) $(CURR_TARGET_DIR)/CastAssert.g $(CURR_INTERMEDIATE_FILES)


MAKE_ANTLR_RULES_G_FILE_NAME      := CastTwoTree.g
MAKE_ANTLR_RULES_LEXER_NAME       :=
MAKE_ANTLR_RULES_PARSER_NAME      := CastTwoTreeParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CastTwoTreeParser
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

$(CURR_TARGET_DIR)/CastTwoTreeParser.java : $(CURR_TARGET_DIR)/CastTwoParser.java

MAKE_ANTLR_RULES_G_FILE_NAME      := DumbTwo.g
MAKE_ANTLR_RULES_LEXER_NAME       := DumbTwoLexer
MAKE_ANTLR_RULES_PARSER_NAME      := DumbTwoParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := DumbTwo
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

CURR_INTERMEDIATE_FILES := $(CURR_TARGET_DIR)/DumbTwoParserTokenTypes.java \
                           $(CURR_TARGET_DIR)/DumbTwoParserTokenTypes.txt \
                           $(CURR_INTERMEDIATE_FILES)
