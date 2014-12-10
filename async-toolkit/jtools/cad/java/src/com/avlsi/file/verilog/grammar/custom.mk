MAKE_ANTLR_RULES_G_FILE_NAME      := verilog.g
MAKE_ANTLR_RULES_LEXER_NAME       := VerilogLexer
MAKE_ANTLR_RULES_PARSER_NAME      := VerilogParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := VerilogTokenTypes

include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk

MAKE_ANTLR_RULES_G_FILE_NAME      := PortDeclaration.g
MAKE_ANTLR_RULES_LEXER_NAME       :=
MAKE_ANTLR_RULES_PARSER_NAME      := PortDeclarationTreeParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := PortDeclarationTokenTypes

include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk
