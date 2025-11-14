# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

MAKE_ANTLR_RULES_G_FILE_NAME      := MathExpression.g
MAKE_ANTLR_RULES_LEXER_NAME       := MathExpressionLexer
MAKE_ANTLR_RULES_PARSER_NAME      := MathExpressionParser
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME := MathExpressionParser
include $(BUILD_SYSTEM_ROOT)/filetypes/antlrfiles/mkantlrrules.mk
