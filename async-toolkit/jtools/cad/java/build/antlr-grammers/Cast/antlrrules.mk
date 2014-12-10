
ANTLR_TARGETS += src/com/avlsi/cast/impl/CastLexer.java                \
                 src/com/avlsi/cast/impl/CastParser.java               \
                 src/com/avlsi/cast/impl/CastTokenTypes.java           \
                 src/com/avlsi/cast/impl/CastTreeParser.java           \
                 src/com/avlsi/cast/impl/CastTreeParserTokenTypes.java \
                 src/com/avlsi/cast/impl/DumbLexer.java                \
                 src/com/avlsi/cast/impl/DumbParser.java               \
                 src/com/avlsi/cast/impl/DumbTokenTypes.java
# Cast.g

src/com/avlsi/cast/impl/CastParser.java \
src/com/avlsi/cast/impl/CastLexer.java \
src/com/avlsi/cast/impl/CastTokenTypes.java: src/com/avlsi/cast/impl/Cast.g
	cd src/com/avlsi/cast/impl && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) Cast.g
# CastTree.g

src/com/avlsi/cast/impl/CastTreeParser.java \
src/com/avlsi/cast/impl/CastTreeParserTokenTypes.java: src/com/avlsi/cast/impl/CastTree.g
	cd src/com/avlsi/cast/impl && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) CastTree.g

# Dumb.g

src/com/avlsi/cast/impl/DumbParser.java \
src/com/avlsi/cast/impl/DumbLexer.java \
src/com/avlsi/cast/impl/DumbTokenTypes.java: src/com/avlsi/cast/impl/Dumb.g
	cd src/com/avlsi/cast/impl && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) Dumb.g
