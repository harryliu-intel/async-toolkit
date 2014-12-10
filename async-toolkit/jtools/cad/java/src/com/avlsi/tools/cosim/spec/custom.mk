MAKE_ANTLR_RULES_G_FILE_NAME      := CoSim.g
MAKE_ANTLR_RULES_LEXER_NAME       := CoSimLexer
MAKE_ANTLR_RULES_PARSER_NAME      := CoSimParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := CoSimParser

include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk
