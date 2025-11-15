# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


ANTLR_TARGETS += src/com/avlsi/tools/cosim/spec/CoSimParser.java           \
                 src/com/avlsi/tools/cosim/spec/CoSimLexer.java            \
                 src/com/avlsi/tools/cosim/spec/CoSimParserTokenTypes.java

src/com/avlsi/tools/cosim/spec/CoSimParser.java \
src/com/avlsi/tools/cosim/spec/CoSimLexer.java \
src/com/avlsi/tools/cosim/spec/CoSimParserTokenTypes.java : src/com/avlsi/tools/cosim/spec/CoSim.g
	cd src/com/avlsi/tools/cosim/spec && $(ANTLR) CoSim.g
