/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.io.Reader;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import antlr.Token;
import antlr.collections.AST;

import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLParser;
import com.avlsi.file.cdl.parser.CDLWalker;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;


public final class ReadCDLIntoFactory {
    /**
     * This class should not be instantiated.
     **/
    private ReadCDLIntoFactory() { }

    public static void readCDL( final Reader r,
                                final CDLFactoryInterface f ) 
    throws RecognitionException, TokenStreamException 
    
    {
        final CDLLexer lexer = new CDLLexer( r, false );
        final CDLParser parser = new CDLParser( lexer );
        parser.setASTNodeClass( CDLParser.ASTWithToken.class.getName() );
        
        parser.goal();
        final AST ast = parser.getAST();
        final CDLWalker walker = new CDLWalker();
        
        walker.goal( ast, new LocalEnvironment(), f );
    }

    public static void readCDLSimple( final Reader r,
                                      final CDLFactoryInterface f)
        throws RecognitionException, TokenStreamException 
    {
        final CDLLexer lexer = new CDLLexer( r, false );
        final CDLParser parser = new CDLParser( lexer );
        final CDLWalker walker = new CDLWalker();

        parser.setASTNodeClass( CDLParser.ASTWithToken.class.getName() );
        
        boolean eof;
        do {
            parser.expr();
            final AST ast = parser.getAST();
            eof = ( ast == null || ast.getType() == Token.EOF_TYPE );
            if(!eof) walker.expr(ast, new LocalEnvironment(), f );
        } while(!eof);
    }
}
