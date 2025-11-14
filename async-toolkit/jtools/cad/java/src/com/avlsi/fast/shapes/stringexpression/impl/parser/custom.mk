# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

MAKE_ANTLR_RULES_G_FILE_NAME      := StringExpression.g
MAKE_ANTLR_RULES_LEXER_NAME       := StringExpressionLexer
MAKE_ANTLR_RULES_PARSER_NAME      := StringExpressionParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := StringExpressionParser
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk
