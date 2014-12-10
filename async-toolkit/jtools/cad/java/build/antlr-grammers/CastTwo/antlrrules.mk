ANTLR_TARGETS += src/com/avlsi/cast2/impl/CastPrsParser.java                \
                 src/com/avlsi/cast2/impl/CastPrsParserTokenTypes.java      \
                 src/com/avlsi/cast2/impl/CastSubcellsParser.java           \
                 src/com/avlsi/cast2/impl/CastSubcellsParserTokenTypes.java \
                 src/com/avlsi/cast2/impl/CastAliasesParser.java           \
                 src/com/avlsi/cast2/impl/CastAliasesParserTokenTypes.java \
                 src/com/avlsi/cast2/impl/CastAssertParser.java           \
                 src/com/avlsi/cast2/impl/CastAssertParserTokenTypes.java \
                 src/com/avlsi/cast2/impl/CastSubtypesParser.java           \
                 src/com/avlsi/cast2/impl/CastSubtypesParserTokenTypes.java \
                 src/com/avlsi/cast2/impl/CastTwoLexer.java                 \
                 src/com/avlsi/cast2/impl/CastTwoParser.java                \
                 src/com/avlsi/cast2/impl/CastTwoTokenTypes.java            \
                 src/com/avlsi/cast2/impl/CastTwoTreeParser.java            \
                 src/com/avlsi/cast2/impl/CastTwoTreeParserTokenTypes.java  \
                 src/com/avlsi/cast2/impl/DumbTwoLexer.java                 \
                 src/com/avlsi/cast2/impl/DumbTwoParser.java                \
                 src/com/avlsi/cast2/impl/DumbTwoTokenTypes.java


# CastPrs.g

src/com/avlsi/cast2/impl/CastPrsParser.java \
src/com/avlsi/cast2/impl/CastPrsParserTokenTypes.java: src/com/avlsi/cast2/impl/CastPrs.g src/com/avlsi/cast2/impl/CastTwoTokenTypes.java
	cd src/com/avlsi/cast2/impl && $(ANTLR) -glib CastTwo.g CastPrs.g


# CastAliases.g

src/com/avlsi/cast2/impl/CastAliasesParser.java \
src/com/avlsi/cast2/impl/CastAliasesParserTokenTypes.java: src/com/avlsi/cast2/impl/CastAliases.g src/com/avlsi/cast2/impl/CastTwoTokenTypes.java
	cd src/com/avlsi/cast2/impl && $(ANTLR) -glib CastTwo.g CastAliases.g

# CastAssert.g

src/com/avlsi/cast2/impl/CastAssertParser.java \
src/com/avlsi/cast2/impl/CastAssertParserTokenTypes.java: src/com/avlsi/cast2/impl/CastAssert.g src/com/avlsi/cast2/impl/CastTwoTokenTypes.java
	cd src/com/avlsi/cast2/impl && $(ANTLR) -glib "CastPrs.g;CastTwo.g" CastAssert.g

# CastSubcells.g

src/com/avlsi/cast2/impl/CastSubcellsParser.java \
src/com/avlsi/cast2/impl/CastSubcellsParserTokenTypes.java: src/com/avlsi/cast2/impl/CastSubcells.g src/com/avlsi/cast2/impl/CastTwoTokenTypes.java
	cd src/com/avlsi/cast2/impl && $(ANTLR) -glib CastTwo.g CastSubcells.g

# CastSubtypes.g

src/com/avlsi/cast2/impl/CastSubtypesParser.java \
src/com/avlsi/cast2/impl/CastSubtypesParserTokenTypes.java: src/com/avlsi/cast2/impl/CastSubtypes.g src/com/avlsi/cast2/impl/CastTwoTokenTypes.java
	cd src/com/avlsi/cast2/impl && $(ANTLR) -glib CastTwo.g CastSubtypes.g

# CastTwo.g

src/com/avlsi/cast2/impl/CastTwoParser.java \
src/com/avlsi/cast2/impl/CastTwoLexer.java \
src/com/avlsi/cast2/impl/CastTwoTokenTypes.java: src/com/avlsi/cast2/impl/CastTwo.g
	cd src/com/avlsi/cast2/impl && $(ANTLR) CastTwo.g

# CastTwoTree.g

src/com/avlsi/cast2/impl/CastTwoTreeParser.java \
src/com/avlsi/cast2/impl/CastTwoTreeParserTokenTypes.java: src/com/avlsi/cast2/impl/CastTwoTree.g  src/com/avlsi/cast2/impl/CastTwoTokenTypes.java
	cd src/com/avlsi/cast2/impl && $(ANTLR) CastTwoTree.g

# DumbTwo.g

src/com/avlsi/cast2/impl/DumbTwoParser.java \
src/com/avlsi/cast2/impl/DumbTwoLexer.java \
src/com/avlsi/cast2/impl/DumbTwoTokenTypes.java: src/com/avlsi/cast2/impl/DumbTwo.g
	cd src/com/avlsi/cast2/impl && $(ANTLR) DumbTwo.g

