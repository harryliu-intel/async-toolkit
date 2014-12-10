MAKE_ANTLR_RULES_G_FILE_NAME      := Cast.g
MAKE_ANTLR_RULES_LEXER_NAME       := CastLexer
MAKE_ANTLR_RULES_PARSER_NAME      := CastParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := Cast

include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

MAKE_ANTLR_RULES_G_FILE_NAME      := Dumb.g
MAKE_ANTLR_RULES_LEXER_NAME       := DumbLexer
MAKE_ANTLR_RULES_PARSER_NAME      := DumbParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := DumbParser

include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

CURR_INTERMEDIATE_FILES := $(CURR_TARGET_DIR)/DumbTokenTypes.java \
                           $(CURR_TARGET_DIR)/DumbTokenTypes.txt \
                           $(CURR_INTERMEDIATE_FILES)

MAKE_ANTLR_RULES_G_FILE_NAME      := CastTree.g
MAKE_ANTLR_RULES_LEXER_NAME       :=
MAKE_ANTLR_RULES_PARSER_NAME      := CastTreeParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CastTreeParser

include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

$(CURR_TARGET_DIR)/CastTreeParser.java: $(CURR_TARGET_DIR)/CastParser.java

