# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

ANTLR_TARGETS += src/com/avlsi/util/mathexpression/impl/parser/MathExpressionParser.java           \
                 src/com/avlsi/util/mathexpression/impl/parser/MathExpressionLexer.java            \
                 src/com/avlsi/util/mathexpression/impl/parser/MathExpressionParserTokenTypes.java


# MathExpression.g

src/com/avlsi/util/mathexpression/impl/parser/MathExpressionParser.java\
src/com/avlsi/util/mathexpression/impl/parser/MathExpressionLexer.java\
src/com/avlsi/util/mathexpression/impl/parser/MathExpressionParserTokenTypes.java: src/com/avlsi/util/mathexpression/impl/parser/MathExpression.g
	cd src/com/avlsi/util/mathexpression/impl/parser && $(ANTLR) MathExpression.g
