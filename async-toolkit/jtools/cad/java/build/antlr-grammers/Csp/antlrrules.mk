# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


ANTLR_TARGETS += src/com/avlsi/csp/grammar/CspParser.java           \
                 src/com/avlsi/csp/grammar/CspLexer.java            \
                 src/com/avlsi/csp/grammar/CspParserTokenTypes.java

src/com/avlsi/csp/grammar/CspParser.java \
src/com/avlsi/csp/grammar/CspLexer.java \
src/com/avlsi/csp/grammar/CspParserTokenTypes.java : src/com/avlsi/csp/grammar/Csp.g
	cd src/com/avlsi/csp/grammar && $(ANTLR) Csp.g
