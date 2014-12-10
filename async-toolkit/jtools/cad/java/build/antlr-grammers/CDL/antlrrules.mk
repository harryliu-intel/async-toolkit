CDL_ANTLR_TARGETS = src/com/avlsi/file/cdl/parser/CDLLexer.java           \
                    src/com/avlsi/file/cdl/parser/CDLParser.java          \
                    src/com/avlsi/file/cdl/parser/CDLWalker.java          \
                    src/com/avlsi/file/cdl/parser/CDLLexerTokenTypes.java
ANTLR_TARGETS += $(CDL_ANTLR_TARGETS)

# CDL.g

$(CDL_ANTLR_TARGETS): src/com/avlsi/file/cdl/parser/CDL.g
	cd src/com/avlsi/file/cdl/parser && $(ANTLR) CDL.g
