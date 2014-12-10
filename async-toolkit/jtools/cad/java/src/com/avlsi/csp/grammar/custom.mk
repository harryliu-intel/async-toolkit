 
MAKE_ANTLR_RULES_G_FILE_NAME      := Csp.g
MAKE_ANTLR_RULES_LEXER_NAME       := CspLexer
MAKE_ANTLR_RULES_PARSER_NAME      := CspParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CspParser

include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk
