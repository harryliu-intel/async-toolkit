//
// Copyright 2001 Asynchronous Digital Design.  All rights reserved.
//
// $Id: //mrl/sw/intel/cad/java/src/com/avlsi/csp/grammar/Csp.g#1 $
//

// authors: Jesse Rosenstock, Tim Crowder
// version: $Revision: #1 $ $Date: 2014/12/10 $

header {
    package com.avlsi.csp.grammar;

    import java.math.BigInteger;
    import java.io.InputStream;
    import java.util.Collection;

    import com.avlsi.util.container.Pair;
    import com.avlsi.cast.impl.TokenWithInfo;
    import com.avlsi.cast2.impl.CastTwoUtil;
    import com.avlsi.csp.ast.ExpressionInterface;
    import com.avlsi.csp.ast.IntegerExpression;
    import com.avlsi.csp.util.CspUtils;
}

class MemParser extends Parser;
options {
    buildAST = false;
    k = 1;
    defaultErrorHandler = false;
}
{
    public static void parseFile(final InputStream s,
                                 final String filename,
                                 final Collection<BigInteger> ints)
    throws Exception {
        final MemLexer lexer = new MemLexer(s);
        lexer.setFilename(filename);
        lexer.setTokenObjectClass(TokenWithInfo.class.getName());
        final MemParser parser = new MemParser(lexer);
        parser.goal(ints);
    }
    public static void main(String[] args) throws Exception {
        for (String arg : args) {
            System.out.println("Parsing " + arg);
            final Collection<BigInteger> ints = new java.util.ArrayList<>();
            parseFile(new java.io.FileInputStream(arg), arg, ints);
            System.out.println(ints);
        }
    }
}
goal [Collection<BigInteger> ints]
    { ExpressionInterface e; }
    : (e=integer {ints.add(CspUtils.getIntegerConstant(e));})* EOF
    ;
integer returns [ExpressionInterface expr = null]
    : i:INTEGER {expr = new IntegerExpression(i.getText(), 16); expr.epr(i);}
    ;

class MemLexer extends Lexer;
options {
    k = 3;
    charVocabulary = '\3' .. '\377';
    testLiterals = false;
}

{
    public Token makeToken(final int t) {
        final Token tok = super.makeToken(t);
        ((TokenWithInfo) tok).setFilename(getFilename());
        return tok;
    }
}

WS : (' ' | '\t' | '\n' { newline(); } | '\r') { _ttype = Token.SKIP; } ;
protected HEX_DIGIT : '0'..'9' | 'a'..'f' | 'A'..'F' ;
INTEGER
    : (HEX_DIGIT)+ ('_'! (HEX_DIGIT)+)*
    ;

COMMENT_1
    : "/*"
       ( options { generateAmbigWarnings=false; }
       : { LA(2) != '/' }? '*'
       | '\n' { newline(); }
       | ~('*' | '\n')
       )*
      "*/"
    { $setType(Token.SKIP); }
    ;

COMMENT_2
    : "//" (~ '\n' )* '\n' { $setType(Token.SKIP); newline(); }
    ;
