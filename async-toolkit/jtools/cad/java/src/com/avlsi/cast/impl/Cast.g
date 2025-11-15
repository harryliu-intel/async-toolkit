//
// Copyright 2000 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

//
// CAST Parser Grammar (for use with antlr http://www.antlr.org/)
//
// author: Jesse Rosenstock
//

header {
    ///////////////////////////////////////////////////////////////////////
    //
    // Copyright 2000 Asynchronous Digital Design.  All rights reserved.
    //
    // Warning:  This file was AUTOMATICALLY GENERATED!!!
    // 
    // DO NOT check in.
    // DO NOT modify.
    //
    // You want to modify Cast.g instead.
    //
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast.impl;
}

// Import the necessary classes
{
    import java.io.*;
    import antlr.TokenStreamSelector;

    import antlr.ASTFactory;
    import com.avlsi.cast.impl.TupleValue;
    import com.avlsi.cast.impl.SemanticWrapperException;
    import com.avlsi.cast.impl.IntValue;
    import com.avlsi.cast.impl.InvalidOperationException;
    import com.avlsi.util.debug.Debug;

}

// todo:
// currently, we cannot support {a, b} = {c, d}
//   because of blocks vs. anonymous arrays issue.
//   this could be fixed by using () for arrays
//   or using lookahead
// clean up dumbBlock -- use { in lookahead, but parse it in dumb.g
// handle bad spec blocks, ie not csp, hse, prs, ...

// flat or tree, that is the question:
//   ie, (op a b c) or (op (op a b) c) via passing lhs into parse of rhs

// better handling of initializers / params in formal param list than [identP]


//---------------------------------------------------------------------------
// Define a Parser, calling it CastParser
//---------------------------------------------------------------------------
class CastParser extends Parser;
options {
    classHeaderSuffix = CastParserInterface;
    k = 2;
    exportVocab = Cast;
    buildAST = true;
    defaultErrorHandler = false;
    ASTLabelType = "ASTWithInfo";
}

tokens {
    ARRAY;
    ARRAY_SUB;
    ARRAY_ACCESS;
    BLOCK;
    BODY_STATEMENT_LIST;
    EXPRESSION;
    EXPRESSION_LIST;
    FIELD_ACCESS;
    FIELD_IDENT;
    FORMALS;
    IF;
    LEVEL_BLOCK;
    LOOP;
    LOOP_AND;
    LOOP_OR;
    PORT_LIST;
    PRS_AND;
    PRS_NOT;
    PRS_OR;
    PRS_EXPRESSION;
    COSIM_EXPRESSION;
    RANGE;
    SELECTION;
    TYPE;
    TYPE_BODY; 
    TYPE_DEFINITION; 
    UNARY_PLUS;
    UNARY_MINUS;
    USER_TYPE;
    VAR_DECL;
    STRING;
}

{
    private TokenStreamSelector selector = null;

    public void setSelector(final TokenStreamSelector selector) {
        this.selector = selector;
    }



    private static AST makeMetasAST( final ASTFactory factory,
        final TupleValue metas ) throws SemanticWrapperException {
        
        final AST metasAST;
        
        metasAST = factory.create(CastTreeParserTokenTypes.EXPRESSION_LIST);
        
        final int length = metas.getSize();
        for (int i=0; i<length; i++) {
            final java.math.BigInteger val;
            try {
                val = ((IntValue) metas.accessTuple(i)).getValue();
                final AST child = factory.create(CastTreeParserTokenTypes.EXPRESSION);
                child.addChild(factory.create(CastTreeParserTokenTypes.NUM_INT, val.toString()));
                metasAST.addChild(child);
            } catch (InvalidOperationException e) {
                Debug.assertTrue(false,"The size of the meta-parameter array shrank.");
            }    
        }
        return metasAST;
    }

    private static AST makeBaseTypeAST( final ASTFactory factory,
        final String cellName,
        final AST metasAST ) {

        final AST result;
        
        result = 
	    factory.create( CastTreeParserTokenTypes.USER_TYPE);
        
        result.addChild(factory.create( CastTreeParserTokenTypes.IDENT, 
                cellName));
	
        result.addChild(metasAST);

        return result;
        
    }


    public static AST makeBaseTypeAST( final String cellName,
        AST metasAST ) {
        
        final ASTFactory factory = new ASTFactory();
        factory.setASTNodeClass(ASTWithInfo.class.getName());

        return makeBaseTypeAST( factory, cellName, metasAST );
        
    }

    public static AST makeBaseTypeAST( final String cellName,
        final TupleValue metas )
	throws SemanticWrapperException{
        
        final ASTFactory factory = new ASTFactory();
        factory.setASTNodeClass(ASTWithInfo.class.getName());
        
        
        final AST metasAST = makeMetasAST( factory, metas );
	
        return makeBaseTypeAST( factory, cellName, metasAST );
        
        
    }



    /**
     * Ensures that the string contains only the letter, digit
     * or _ characters.  Throws RecognitionException if this is not 
     * the case.  This is because IDENT needs to accept
     * them inside quoted strings to handle the current way
     * imports are done.  The cast grammar should be fixed.
     **/
    private void checkIdent(final Token t) throws RecognitionException {
        final TokenWithInfo tt = (TokenWithInfo) t;
        final String s = tt.getText();

        for (int i = 0; i < s.length(); ++i) {
            final char ch = s.charAt(i);

            if (!(   'a' <= ch && ch <= 'z'
                  || 'A' <= ch && ch <= 'Z'
                  || '0' <= ch && ch <= '9'
                  || ch == '_')) {
                final RecognitionException re
                    = new RecognitionException("Illegal char " + ch
                        + " in identifier \"" + s + '"',
                        tt.getFilename(), tt.getLine(), tt.getColumn());
                re.column = tt.getColumn();
                throw re;
            }
        }
    }

}

goal 
    {
        if (selector == null) {
            // XXX: throw exception
            System.err.println("selector not set");
            System.exit(1);
        }
    }
    : compilationUnit
    ;

compilationUnit 
    : ( importDeclaration )* ( bodyStatement | typeDeclaration )*
    ;

importDeclaration 
    : IMPORT^ IDENT SEMI!
    ;

bodyStatements
    : ( bodyStatement )*
    { #bodyStatements = #( [BODY_STATEMENT_LIST], #bodyStatements ); }
    ;

bodyStatement 
    // lookahead is used here to avoid having keywords for level / csp / ...
    : prsBlock
    | specBlock
    | javaBlock
    | ( IDENT LCURLY ) => dumbBlock
    | ( IDENT LPAREN IDENT RPAREN LCURLY ) => levelBlock
    | typeBody
    | variableDeclarationStatement
    | loopStatement
    | ifStatement
    | assignmentStatement
    ;

typeDeclaration
    : typeHeader typeBody
    { #typeDeclaration = #( [TYPE_DEFINITION], #typeDeclaration ); }
    ;

typeHeader 
    : DEFINE! id:IDENT {checkIdent(id);}
        LPAREN! parameterList RPAREN!
        LPAREN! parameterList RPAREN!
    ;

parameterList
    : ( v:variableDeclaration[true] ( SEMI! variableDeclaration[true] )* )?
    { #parameterList = #( [FORMALS], #parameterList ); 
      if (#v != null)
          #parameterList.copyInfo(#v); }
    ;

typeBody 
    : lc:LCURLY^ {#lc.setType(BLOCK);} bodyStatements RCURLY!
    ;

instantiation
    : variableDeclaration[false]
    ;

variableDeclarationStatement 
    : variableDeclaration[false] SEMI!
    ;

variableDeclaration![boolean formalP]
    : ( { !formalP }? ( i:INLINE | i1:INLINE1 | f:FLATTEN ) )? 
        t:type v:variableDeclarators[formalP, #t, #i, #i1, #f]
    { #variableDeclaration = #v; }
    ;

variableDeclarators[boolean formalP, AST t, AST inlineAST, AST inline1AST,
                    AST flattenAST]
    : variableDeclarator[formalP, getASTFactory().dupTree(t),
                         getASTFactory().dupTree(inlineAST),
                         getASTFactory().dupTree(inline1AST),
                         getASTFactory().dupTree(flattenAST)]
        ( COMMA! variableDeclarator[formalP, getASTFactory().dupTree(t),
                                    getASTFactory().dupTree(inlineAST),
                                    getASTFactory().dupTree(inline1AST),
                                    getASTFactory().dupTree(flattenAST)] )*
    ;

variableDeclarator![boolean formalP, AST t, AST inlineAST, AST inline1AST,
                    AST flattenAST]
    : id:IDENT {checkIdent(id);} ( a:arraySelector )?
      ( { !formalP }? vi:variableInitializer )?
    { #variableDeclarator
        = #( [VAR_DECL], id, inlineAST, inline1AST, flattenAST, #([TYPE], t, a), vi ); }
    ;

variableInitializer
    : lp:LPAREN^ {#lp.setType(PORT_LIST);} expressionList RPAREN!
    | ASSIGN^ expression
    ;

arraySelector
    : lb:LBRACK^ {#lb.setType(ARRAY_SUB);} ranges RBRACK!
    ;

ranges 
    : range ( COMMA! range )*
    ;

range
    : expression ( DOTDOT! expression )?
    { #range = #( [RANGE], range ); }
    ;

type
    : primitiveType
    | id:IDENT {checkIdent(id);} typeParameterList { #type = #( [USER_TYPE], #type ); }
    ;

typeParameterList 
    : LPAREN! expressionList RPAREN!
    |! /* empty */ {  #typeParameterList = #( [EXPRESSION_LIST], #typeParameterList ) ; }
    ;

primitiveType 
    : BOOL
    | FLOAT
    | INT
    | NODE
    ;

loopStatement 
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT {checkIdent(id);} COLON! range COLON! bodyStatements GT!
    ;

ifStatement 
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! bodyStatements RBRACK!
    ;

assignmentStatement 
    : assignment SEMI!
    ;

assignment 
    : assignableExpression ( ASSIGN! expression )+
    { #assignment = #( [ASSIGN], #assignment ); }
    ;

// fix me up later!
// ugh, duplication
assignableExpression
    : id:IDENT {checkIdent(id);}
      (! i:DOT_IDENT {checkIdent(i);}
         { final String s = i.getText();
           #assignableExpression = #( [FIELD_ACCESS],
                                          #assignableExpression,
                                          #[ FIELD_IDENT, s ] ); }
      |! as:arraySelector
         { #assignableExpression = #( [ARRAY_ACCESS], #assignableExpression, #as ); }
      )*
    ;

dumbBlock!
    : id:IDENT {checkIdent(id);} LCURLY
    {
        final String[] a = new String[] {
            "asp", "aspice", "csp", "hse", "sprs"
        };

        int i = 0;
        for (; i < a.length; ++i)
            if (a[i].equals(id.getText()))
                break;

        if (i == a.length)
            System.err.println("Unknown body " + id.getText() + " ignored.");

        selector.push("dumblexer");
        new DumbParser(getInputState()).goal();
        selector.pop();
    }
    ;

levelBlock!
    : id1:IDENT LPAREN id2:IDENT RPAREN tb:typeBody
    {
        if (!"level".equals(id1.getText())) {
            // XXX: throw exception
            System.err.println(id1.getText() + " is not level");
            System.exit(1);
        }
        checkIdent(id2);

        #levelBlock = #([LEVEL_BLOCK], id2, tb);
    }
    ;

// hack, to support X x(a,b,,,,).
// urk, accidentally supports ,, where we don't want to
expressionList
    : ( /* empty */
      | expression ( COMMA! expressionListExpression )*
      ) { #expressionList = #( [EXPRESSION_LIST], #expressionList); }
    // commented out version causes non-determinism, above should
    // be equivalent
    //: ( expressionListExpression ( COMMA! expressionListExpression )* )?
    ;

// hack , as above
expressionListExpression
    :! /* empty,  */
        { #expressionListExpression = #( [EXPRESSION], #[IDENT, "_"] ); }
    | expression
    ;

expression 
    : co:conditionalOrExpression
    {
        #expression = #( [EXPRESSION], #expression);
        #expression.copyInfo(#co);
    }
    ;

conditionalOrExpression 
    : conditionalAndExpression ( OR^ conditionalAndExpression )*
    ;

conditionalAndExpression 
    : relationalExpression ( AND^ relationalExpression )*
    ;

relationalExpression 
    : additiveExpression ( ( EQ^ | NE^ | LT^ | LE^ | GT^ | GE^ ) additiveExpression )*
    ;

additiveExpression 
    : multiplicativeExpression ( ( PLUS^ | MINUS^ ) multiplicativeExpression )*
    ;

multiplicativeExpression 
    : unaryExpression ( ( TIMES^ | DIV^ | MOD^ ) unaryExpression )*
    ;

unaryExpression 
    : PLUS^ {#PLUS.setType(UNARY_PLUS);} unaryExpression
    | MINUS^ {#MINUS.setType(UNARY_MINUS);} unaryExpression
    | NOT^ unaryExpression
    | selectionExpression
    ;

// treeify me
// ugh, duplication
selectionExpression
    : primaryExpression
      (! i:DOT_IDENT {checkIdent(i);}
         { final String s = i.getText();
           #selectionExpression = #( [FIELD_ACCESS], #selectionExpression, #[ FIELD_IDENT, s ] ); #selectionExpression.copyInfo(#i); }
      |! as:arraySelector
         { #selectionExpression = #( [ARRAY_ACCESS], #selectionExpression, #as ); #selectionExpression.copyInfo(#as); }
      )*
    ;

primaryExpression
    : id:IDENT {checkIdent(id);}
    | NUM_INT
    | NUM_REAL
    | TRUE
    | FALSE
    | anonymousArray
    | LPAREN! conditionalOrExpression RPAREN!
    ;

/*
selector![AST lhs]
    : i:DOT_IDENT { final String s = i.getText();
                    #selector = #( [FIELD_ACCESS], #lhs, #[ FIELD_IDENT, s ] ); }
    | as:arraySelector { #selector = #( [ARRAY_ACCESS], #lhs, #as ); }
    ;
*/

anonymousArray 
    : lc:LCURLY^ {#lc.setType(ARRAY);} expressionList RCURLY!
    ;

//
// PRS stuff
//

prsBlock
    : PRS^ LCURLY! prsStatements RCURLY!
    ;

// an env block that occurs in a prs block
prsEnvBlock
    : ENV^ LCURLY! prsStatements RCURLY!
    ;

prsStatements
    : ( prsStatement )*
    { #prsStatements = #( [BODY_STATEMENT_LIST], #prsStatements ); }
    ;

prsStatement
    : prsIfStatement
    | prsLoopStatement
    | prsAction
    | prsEnvBlock
    ;

prsIfStatement
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! prsStatements RBRACK!
    ;

prsLoopStatement 
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT {checkIdent(id);}
       COLON! range COLON! prsStatements GT!
    ;

prsAction
    : prsModifiers
      prsExpression ( ARROW^ | CELEM_ARROW^ | COMB_ARROW^ )
      prsNodeExpression ( PLUS | MINUS )
    ;

prsModifiers
    : ( UNSTAB )? ( AFTER expression )? 
    ;

prsExpression
    : poe:prsOrExpression
    {
        #prsExpression = #( [PRS_EXPRESSION], #prsExpression);
        #prsExpression.copyInfo(#poe);
    }
    ;

prsOrExpression
    : prsAndExpression ( o:OR^ {#o.setType(PRS_OR);} prsAndExpression )*
    ;

prsAndExpression
    : prsNotExpression ( a:AND^ {#a.setType(PRS_AND);} prsNotExpression )*
    ;

prsNotExpression
    : n:NOT^ {#n.setType(PRS_NOT);} prsNotExpression
    | prsLoopedExpression
    ;

prsLoopedExpression
    // XXX: LT_AND / LT_OR are hacks!  We should fix this!
    // we want: LT ( AND | OR )
    : ( la:LT_AND^ {#la.setType(LOOP_AND);}
      | lo:LT_OR^  {#lo.setType(LOOP_OR); } )
      id:IDENT {checkIdent(id);} COLON! range COLON! prsExpression GT!
    | prsSelectionExpression
    ;

// review, this allows (foo | bar).baz[0], too general!
prsSelectionExpression
//ugh, duplication
    : prsPrimaryExpression
      (! id:DOT_IDENT {checkIdent(id);}
         { final String s = id.getText();
           #prsSelectionExpression
               = #( [FIELD_ACCESS],
                   #prsSelectionExpression, #[ FIELD_IDENT, s ] ); }
      |! as:arraySelector
         { #prsSelectionExpression
             = #( [ARRAY_ACCESS], #prsSelectionExpression, #as ); }
      )*
    ;

prsPrimaryExpression
    : IDENT
    | LPAREN! prsOrExpression RPAREN!
    ;

prsNodeExpression
//ugh, duplication
    : IDENT
      (! id:DOT_IDENT {checkIdent(id);}
         { final String s = id.getText();
           #prsNodeExpression
               = #( [FIELD_ACCESS],
                   #prsNodeExpression, #[ FIELD_IDENT, s ] ); }
      |! as:arraySelector
         { #prsNodeExpression
             = #( [ARRAY_ACCESS], #prsNodeExpression, #as ); }
      )*
    ;

// a Java block to specify co-sim parameters
javaBlock
    : JAVA^ LCURLY! javaStatements RCURLY!
    ;

javaStatements
    : ( javaStatement )*
    { #javaStatements = #( [BODY_STATEMENT_LIST], #javaStatements ); }
    ;

javaStatement
    : javaClassStatement
    | inputChannelStatement
    | outputChannelStatement
    | internalChannelStatement
    ;

javaClassStatement
    : JAVACLASS^ QuotedString
    ;

inputChannelStatement
    : INPUTCHAN^ QuotedString
    ;

outputChannelStatement
    : OUTPUTCHAN^ QuotedString
    ;

internalChannelStatement
    : INTERNALCHAN^ QuotedString
    ;


//
// SPEC stuff
//

specBlock
    : SPEC^ LCURLY! specStatements RCURLY!
    ;

specStatements
    : ( specStatement )*
    ;

specStatement
    : specExclStatement
    | specAttribStatement
    ;

specExclStatement
    : ( EXCLHI^ | EXCLLO^ ) LPAREN! specNodeList RPAREN!
    ;

specAttribStatement!
    : ATTRIB IDENT IDENT LPAREN! specNodeList SEMI! prsNodeExpression RPAREN!
    { #specAttribStatement = #( [ATTRIB] ); }
    ;

specNodeList
    : prsNodeExpression ( COMMA! prsNodeExpression )*
    ;

//---------------------------------------------------------------------------
// The Cast scanner
//---------------------------------------------------------------------------

class CastLexer extends Lexer;

options {
    classHeaderSuffix = CastLexerInterface;
    charVocabulary = '\0'..'\377';
    exportVocab = Cast;   // call the vocabulary "Cast"
    testLiterals = true;   // automatically test for literals
    k = 2;                  // two characters of lookahead
    caseSensitive = true;
    caseSensitiveLiterals = true;
    defaultErrorHandler = false;
}

tokens {
    BOOL            = "bool"            ;
    DEFINE          = "define"          ;
    FLOAT           = "float"           ;
    FLATTEN         = "flatten"         ;
    IMPORT          = "import"          ;
    INLINE          = "inline"          ;
    INLINE1         = "inline1"         ;
    INT             = "int"             ;
    NODE            = "node"            ;
    TRUE            = "true"            ;
    FALSE           = "false"           ;
    ENV             = "env"             ;
    // prs
    PRS             = "prs"             ;
    AFTER           = "after"           ;
    UNSTAB          = "unstab"          ;
    // java
    JAVA	    = "java"		;
    JAVACLASS	    = "javaclass"	;
    INPUTCHAN       = "inputchannel"    ;
    OUTPUTCHAN      = "outputchannel"   ;
    INTERNALCHAN    = "internalchannel" ;
    // spec
    SPEC            = "spec"            ;
    EXCLHI          = "exclhi"          ;
    EXCLLO          = "excllo"          ;
    ATTRIB          = "attrib"          ;
}

{
    public Token makeToken(final int t) {
        final Token tok = super.makeToken(t);
        tok.setColumn(getColumn());
        ((TokenWithInfo) tok).setFilename(getFilename());
        return tok;
    }
}

//---------------------------------------------------------------------------
// OPERATORS
//---------------------------------------------------------------------------

ARROW           : "->"  ;
PLUS            : '+'   ;
MINUS           : '-'   ;
TIMES           : '*'   ;
DIV             : '/'   ;
MOD             : '%'   ;
AND             : '&'   ;
OR              : '|'   ;
NOT             : '~'   ;
COMMA           : ','   ;
SEMI            : ';'   ;
COLON           : ':'   ;
ASSIGN          : '='   ;
EQ              : "=="  ;
NE              : "!="  ;
LT              : '<'   ;
LE              : "<="  ;
GE              : ">="  ;
GT              : '>'   ;
LPAREN          : '('   ;
RPAREN          : ')'   ;
LCURLY          : '{'   ;
RCURLY          : '}'   ;
LBRACK          : '['   ;
RBRACK          : ']'   ;
//DOT             : '.'   ;
DOTDOT          : ".."  ;
CELEM_ARROW         : "#>"  ;
COMB_ARROW      : "=>"  ;
LT_AND          : "<&"  ;
LT_OR           : "<|"  ;

WS
    : ( ' ' | '\t' | '\f' | ( '\n' { newline(); } ) )
    { $setType(Token.SKIP); }
    ;

COMMENT_1
        : "/*"
           ( options { generateAmbigWarnings=false; }
           :    { LA(2) != '/' }? '*'
           |    '\n'            {newline();}
           |   ~('*' | '\n' | '\r')
           )*
          "*/"
        {$setType(Token.SKIP);}
    ;

COMMENT_2
    : "//" (~ '\n' )* '\n'
      { $setType(Token.SKIP); newline(); }
    ;

// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
// XXX: need to fix the "ident" part.  This should only accept real
// identifiers.  When import is changed to be 'import foo/bar' or 
// 'import foo.bar', we can clamp down on what is accepted.
// For now we'll use the horrible hack checkIdent()
IDENT
    options {testLiterals=true;}
    : ( LETTER | '_' ) ( LETTER | '_' | DIGIT )*
    | '"' ( LETTER | DIGIT | '_' | '/' | '.' | '*' )+ '"'
      { 
          final String s = $getText;
          final String ss = s.substring(1, s.length() - 1);
          $setText(ss);
      }
    ;

// a numeric literal or an identifier
NUM_INT
    { boolean identP = false; }
    :   ( DIGIT )+ ( IDENT { $setType(IDENT); identP = true; } )?
        (   { LA(2) != '.' && LA(3) != '.' && !identP}?
            '.' ( DIGIT )+ ( EXPONENT )?
            { $setType(NUM_REAL); }
        )?
    ;

DOT_IDENT
    : '.' ( IDENT | NUM_INT )
    {
        final String s = $getText;
        final String ss = s.substring(1);
        $setText(ss);
    }
    ;

QuotedString
    : '\'' (~'\'')* '\''
      {
          // strip leading and trailing single quotes
          final String s = $getText;
          final String ss = s.substring(1, s.length() - 1);
          $setText(ss);
      }
    ;

protected
EXPONENT
    : ( 'e' | 'E' ) ( '+' | '-' )? ( DIGIT )+
    ;

protected
LETTER
    : 'a'..'z' | 'A'..'Z'
    ;

protected
DIGIT
    : '0'..'9'
    ;
