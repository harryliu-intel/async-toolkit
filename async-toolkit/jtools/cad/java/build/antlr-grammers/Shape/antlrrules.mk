# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

ANTLR_TARGETS += src/com/avlsi/fast/shapes/impl/parser/ShapeLexer.java           \
                 src/com/avlsi/fast/shapes/impl/parser/ShapeParser.java          \
                 src/com/avlsi/fast/shapes/impl/parser/ShapeLexerTokenTypes.java


# Shape.g

src/com/avlsi/fast/shapes/impl/parser/ShapeLexer.java\
src/com/avlsi/fast/shapes/impl/parser/ShapeParser.java\
src/com/avlsi/fast/shapes/impl/parser/ShapeLexerTokenTypes.java: src/com/avlsi/fast/shapes/impl/parser/Shape.g
	cd src/com/avlsi/fast/shapes/impl/parser && $(ANTLR) Shape.g
