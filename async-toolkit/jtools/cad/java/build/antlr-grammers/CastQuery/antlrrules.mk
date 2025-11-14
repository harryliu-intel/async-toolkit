# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

CASTQUERY_ANTLR_TARGETS = src/com/avlsi/tools/jauto/CastQueryLexer.java \
                          src/com/avlsi/tools/jauto/CastQueryParser.java \
                          src/com/avlsi/tools/jauto/CastQueryParserTokenTypes.java

ANTLR_TARGETS += $(CASTQUERY_ANTLR_TARGETS)

# CastQuery.g

$(CASTQUERY_ANTLR_TARGETS): src/com/avlsi/tools/jauto/CastQuery.g
	cd src/com/avlsi/tools/jauto && $(ANTLR) CastQuery.g
