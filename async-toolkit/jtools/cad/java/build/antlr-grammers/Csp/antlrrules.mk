
ANTLR_TARGETS += src/com/avlsi/csp/grammar/CspParser.java           \
                 src/com/avlsi/csp/grammar/CspLexer.java            \
                 src/com/avlsi/csp/grammar/CspParserTokenTypes.java

src/com/avlsi/csp/grammar/CspParser.java \
src/com/avlsi/csp/grammar/CspLexer.java \
src/com/avlsi/csp/grammar/CspParserTokenTypes.java : src/com/avlsi/csp/grammar/Csp.g
	cd src/com/avlsi/csp/grammar && $(ANTLR) Csp.g
