# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


ANTLR_TARGETS += src/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpressionParser.java           \
                 src/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpressionLexer.java            \
                 src/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpressionParserTokenTypes.java

# StringExpression.g

src/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpressionParser.java\
src/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpressionLexer.java\
src/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpressionParserTokenTypes.java: src/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpression.g
	cd src/com/avlsi/fast/shapes/stringexpression/impl/parser && $(ANTLR) StringExpression.g
