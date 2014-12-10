
ANTLR_TARGETS += src/com/avlsi/tools/cosim/spec/CoSimParser.java           \
                 src/com/avlsi/tools/cosim/spec/CoSimLexer.java            \
                 src/com/avlsi/tools/cosim/spec/CoSimParserTokenTypes.java

src/com/avlsi/tools/cosim/spec/CoSimParser.java \
src/com/avlsi/tools/cosim/spec/CoSimLexer.java \
src/com/avlsi/tools/cosim/spec/CoSimParserTokenTypes.java : src/com/avlsi/tools/cosim/spec/CoSim.g
	cd src/com/avlsi/tools/cosim/spec && $(ANTLR) CoSim.g
