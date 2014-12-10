CASTQUERY_ANTLR_TARGETS = src/com/avlsi/tools/jauto/CastQueryLexer.java \
                          src/com/avlsi/tools/jauto/CastQueryParser.java \
                          src/com/avlsi/tools/jauto/CastQueryParserTokenTypes.java

ANTLR_TARGETS += $(CASTQUERY_ANTLR_TARGETS)

# CastQuery.g

$(CASTQUERY_ANTLR_TARGETS): src/com/avlsi/tools/jauto/CastQuery.g
	cd src/com/avlsi/tools/jauto && $(ANTLR) CastQuery.g
