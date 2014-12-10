MAKE_ANTLR_RULES_G_FILE_NAME      := Shape.g
MAKE_ANTLR_RULES_LEXER_NAME       := ShapeLexer
MAKE_ANTLR_RULES_PARSER_NAME      := ShapeParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := ShapeLexer
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk
