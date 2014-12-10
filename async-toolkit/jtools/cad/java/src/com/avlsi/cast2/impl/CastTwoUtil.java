/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/cast/impl/CastParserInterface.jav
a#4 $
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.impl;

import java.io.StringReader;
import java.math.BigInteger;

import antlr.ASTFactory;
import antlr.CharScanner;
import antlr.Parser;
import antlr.RecognitionException;
import antlr.TokenStream;
import antlr.TokenStreamException;
import antlr.TokenStreamSelector;
import antlr.collections.AST;

import com.avlsi.cast.impl.ASTWithInfo;
import com.avlsi.cast.impl.IntValue;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.SemanticWrapperException;
import com.avlsi.cast.impl.TokenWithInfo;
import com.avlsi.cast.impl.TupleValue;
import com.avlsi.util.functions.UnaryFunction;

public class CastTwoUtil {
    /**
     * This class should not be instantiated.
     **/
    private CastTwoUtil() { }

    interface ParserFactory {
        CastTwoParserInterface getParser(TokenStream stream);
    }

    interface ParserCallback {
        void bodyStatement(ASTWithInfo ast) throws RecognitionException;
        void ifStart(ASTWithInfo expr) throws RecognitionException;
        void ifEnd() throws RecognitionException;
        void loopStart(ASTWithInfo ident, ASTWithInfo range)
            throws RecognitionException;
        void loopEnd() throws RecognitionException;
    }

    static final ParserFactory PRS_PARSER = new ParserFactory() {
        public CastTwoParserInterface getParser(TokenStream stream) {
            return new CastPrsParser(stream);
        }
    };

    static final ParserFactory SUBCELLS_PARSER = new ParserFactory() {
        public CastTwoParserInterface getParser(TokenStream stream) {
            return new CastSubcellsParser(stream);
        }
    };

    static final ParserFactory SUBTYPES_PARSER = new ParserFactory() {
        public CastTwoParserInterface getParser(TokenStream stream) {
            return new CastSubtypesParser(stream);
        }
    };

    static final ParserFactory ASSERT_PARSER = new ParserFactory() {
        public CastTwoParserInterface getParser(TokenStream stream) {
            return new CastAssertParser(stream);
        }
    };

    static final ParserFactory ALIASES_PARSER = new ParserFactory() {
        public CastTwoParserInterface getParser(TokenStream stream) {
            return new CastAliasesParser(stream);
        }
    };

    public static TokenStreamSelector getSelector(final CharScanner lexer,
                                                  final String lexerName,
                                                  final int lineNum,
                                                  final int columnNum,
                                                  final String fileName) {
        lexer.setLine(lineNum);
        lexer.setColumn(columnNum);
        lexer.setFilename(fileName);
        final DumbTwoLexer dumbLexer = new DumbTwoLexer(lexer.getInputState());

        final TokenStreamSelector selector = new TokenStreamSelector();
        selector.addInputStream(lexer, lexerName);
        selector.addInputStream(dumbLexer, "dumblexer");
        selector.select(lexer);
        return selector;
    }

    static TokenStreamSelector getSelector(String s, int lineNum,
                                           int columnNum, String fileName) {
        final StringReader input = new StringReader(s);
        final CastTwoLexer castLexer = new CastTwoLexer(input);
        castLexer.setTokenObjectClass(TokenWithInfo.class.getName());

        return getSelector(castLexer, "castlexer", lineNum, columnNum, fileName);
    }

    public static String getDumbBlockString(final TokenStreamSelector selector,
                                            final Parser current)
    throws RecognitionException, TokenStreamException {
        selector.push("dumblexer");
        final String dum = new DumbTwoParser(current.getInputState()).goal();
        selector.pop();
        return dum;
    }

    /**
     * Parse the given string using the specified subparser, and return the AST.
     **/
    static AST parseStringAs(String s, int lineNum, int columnNum,
                             String fileName, boolean verbose,
                             ParserFactory factory)
        throws RecognitionException, TokenStreamException {
        final TokenStreamSelector selector =
            getSelector(s, lineNum, columnNum, fileName);
        final CastTwoParserInterface parser = factory.getParser(selector);
        parser.setVerbosity(verbose);
        parser.setSelector(selector);
        parser.setFilename(fileName);
        parser.setASTNodeClass(ASTWithInfo.class.getName());
        parser.goal();
        return parser.getAST();
    }

    /**
     * Parse the given string using the specified subparser, the call the
     * specified callback object.
     **/
    static void parseStringAs(String s, int lineNum, int columnNum,
                              String fileName, boolean verbose,
                              ParserFactory factory,
                              ParserCallback cb)
        throws RecognitionException, TokenStreamException {
        final TokenStreamSelector selector =
            getSelector(s, lineNum, columnNum, fileName);
        final CastTwoParserCallback parser =
            (CastTwoParserCallback) factory.getParser(selector);
        parser.setVerbosity(verbose);
        parser.setSelector(selector);
        parser.setFilename(fileName);
        parser.setASTNodeClass(ASTWithInfo.class.getName());
        parser.goalCallback(cb);
    }

    private static ASTWithInfo makeMetasAST( final ASTFactory factory,
        final TupleValue metas ) throws SemanticWrapperException {
        
        final AST metasAST;
        
        metasAST = factory.create(CastTwoTreeParser.EXPRESSION_LIST);
        
        final int length = metas.getSize();
        for (int i=0; i<length; i++) {
            final java.math.BigInteger val;
            try {
                val = ((IntValue) metas.accessTuple(i)).getValue();
                final AST child = factory.create(CastTwoTreeParser.EXPRESSION);
                child.addChild(factory.create(CastTwoTreeParser.NUM_INT, val.toString()));
                metasAST.addChild(child);
            } catch (InvalidOperationException e) {
                throw new AssertionError("The size of the meta-parameter array shrank.");
            }    
        }
        return (ASTWithInfo) metasAST;
    }

    /**
     * The created tree gets its file/line/column info from errorAST.
     **/
    private static ASTWithInfo makeBaseTypeAST( final ASTFactory factory,
        final String cellName,
        final ASTWithInfo metasAST,
        final ASTWithInfo errorAST ) {

        final ASTWithInfo result;
        final ASTWithInfo child;

        result = (ASTWithInfo) factory.create(CastTwoTreeParser.USER_TYPE);
        result.copyInfo(errorAST);

        child = (ASTWithInfo) factory.create(CastTwoTreeParser.IDENT, cellName);
        child.copyInfo(errorAST);
        result.addChild(child);

        result.addChild(metasAST);

        return result;
        
    }

    static AST makeBaseTypeAST( final String cellName,
        ASTWithInfo metasAST,
        final ASTWithInfo errorAST ) {

        final ASTFactory factory = new ASTFactory();
        factory.setASTNodeClass(ASTWithInfo.class.getName());

        if ( metasAST == null ) {
            metasAST = (ASTWithInfo) factory.create(CastTwoTreeParser.EXPRESSION_LIST);
        }

        return makeBaseTypeAST( factory, cellName, metasAST,
                                errorAST == null ? metasAST : errorAST );

    }

    static AST makeBaseTypeAST( final String cellName,
        final TupleValue metas,
        final ASTWithInfo errorAST ) throws SemanticWrapperException {

        final ASTFactory factory = new ASTFactory();
        factory.setASTNodeClass(ASTWithInfo.class.getName());

        
        final ASTWithInfo metasAST = makeMetasAST( factory, metas );

        return makeBaseTypeAST( factory, cellName, metasAST,
                                errorAST == null ? metasAST : errorAST );
    }

    /**
     * Returns the value of the string for a NUM_INT or NUM_HEX as a
     * BigInteger.
     **/
    static BigInteger bigIntegerValue(final String s) {
        if (s.startsWith("0x"))
            return new BigInteger(s.substring(2), 16);
        else
            return new BigInteger(s, 10);
    }
}
