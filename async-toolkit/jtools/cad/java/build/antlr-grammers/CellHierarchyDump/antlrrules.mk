
ANTLR_TARGETS += src/com/avlsi/tools/jauto/CellHierarchyDumpLexer.java  \
                 src/com/avlsi/tools/jauto/CellHierarchyDumpParser.java

# CellHierarchyDump.g

src/com/avlsi/tools/jauto/CellHierarchyDumpLexer.java\
src/com/avlsi/tools/jauto/CellHierarchyDumpParser.java : src/com/avlsi/tools/jauto/CellHierarchyDump.g
	cd src/com/avlsi/tools/jauto && $(ANTLR) CellHierarchyDump.g
