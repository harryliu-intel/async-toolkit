package com.avlsi.tools.cell;

import antlr.collections.AST;
import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.impl.ASTWithInfo;
import com.avlsi.cast.impl.CastParserEnvironment;
import com.avlsi.cast.impl.CastTreeParserInterface;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentEntry;
import com.avlsi.cast.impl.EnvironmentEntryIterator;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast.impl.UserDefinedValue;
import com.avlsi.cast.impl.SemanticWrapperException;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.TupleValue;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.common.HierName;

import com.avlsi.io.FileSearchPath;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

/**
 * A class to access cell level constants.
 **/
public class CellConstants {
    private final CastParserEnvironment cpe;

    public CellConstants(final CastParserEnvironment cpe) {
        this.cpe = cpe;
    }

    public CellConstants(final CastFileParser cpe) {
        this(cpe.getParserEnvironment());
    }

    /**
     * Return the top level constants accessible in the specified cell.
     *
     * @param cellName cell to get constants for
     * @return constants and their values represented as key value pairs
     **/
    public Environment getTopLevelConstants(final String cellName)
        throws RecognitionException, TokenStreamException {
        final Environment env = NullEnvironment.getInstance();
        final CellInterface ci = cpe.getCell(env, cellName, null, null);

        final String type = ci.getFullyQualifiedType();
        final CastTreeParserInterface parser = cpe.getTreeParser();
        parser.setCastParserEnvironment(cpe);
        // XXX: assumes CASTv2
        final UserDefinedValue udv = (UserDefinedValue)
            ((CastTwoTreeParser) parser).getFullyQualifiedCellValue(
                Symbol.create(CellUtils.getBaseType(type)),
                env, new ASTWithInfo());

        final CastTwoParser castParser =
            CastTwoParser.getParser(cellName, 0, 0, "<no name>");
        castParser.cellType();
        final AST ast = castParser.getAST();
        final AST metas = ast.getFirstChild().getNextSibling();
        final CastTwoTreeParser treeParser = new CastTwoTreeParser();
        final TupleValue tv;
        if (metas == null) {
            tv = new TupleValue(new Value[0]);
        } else {
            tv = treeParser.expressionList(metas, env, false);
        }

        return udv.getCellConstants(tv);
    }

    public static void main(final String args[]) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;
        final String castPath = theArgs.getArgValue("cast-path", ".");
        final String cellName = theArgs.getArgValue("cell", null);
        final CastFileParser castParser =
            new CastFileParser(new FileSearchPath(castPath), "2");
        final CellConstants consts = new CellConstants(castParser);
        for (EnvironmentEntryIterator i =
                consts.getTopLevelConstants(cellName).entryIterator();
             i.hasNext(); ) {
            final EnvironmentEntry entry = i.next();
            System.out.println(entry.getName() + " = " + entry.getValue());
        }
    }
}
