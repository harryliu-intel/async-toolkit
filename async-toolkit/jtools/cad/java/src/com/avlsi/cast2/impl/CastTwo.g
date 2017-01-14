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
    // You want to modify CastTwo.g instead.
    //
    // This parses version two of cast (fast, castv2, whatever it's
    // being called).
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;
    
}

// Import the necessary classes
{
    /* It's best to avoid importing any classes, because antlr will not
     * automatically replicate this section when emitting the extended .g files
     * for grammar inheritance.  This means that you would have to go edit the
     * .g files for all the subparsers, and also add the import line there,
     * which is inconvenient.
     */
}

// todo:
// currently, we cannot support {a, b} = {c, d}
//   because of blocks vs. anonymous arrays issue.
//   this could be fixed by using () for arrays
//   or using lookahead

// flat or tree, that is the question:
//   ie, (op a b c) or (op (op a b) c) via passing lhs into parse of rhs

// better handling of initializers / params in formal param list than [identP]


//---------------------------------------------------------------------------
// Define a Parser, calling it CastTwoParser
//---------------------------------------------------------------------------
class CastTwoParser extends Parser;
options {
    classHeaderSuffix = CastTwoParserInterface;
    k = 2;    // This is a problem if set too high
    exportVocab = CastTwo;
    buildAST = true;
    defaultErrorHandler = false;
    ASTLabelType = "com.avlsi.cast.impl.ASTWithInfo";
}

tokens {
    ALINT_FANIN;
    ARRAY;
    ARRAY_SUB;
    ARRAY_ACCESS;
    BLOCK;
    BODY_STATEMENT_LIST;
    CHANNEL_TIMING_INFO;
    CHANNEL_WIDTH;
    CSP_PORTS;
    DUMB_BLOCK;
    EXPRESSION;
    EXPRESSION_LIST;
    FIELD_ACCESS;
    FIELD_IDENT;
    FORMALS;
    IF;
    IMPORT_IDENT;
    INHERITANCE;
    INHERITANCE_LIST;
    JAVA_CLASS_INITIALIZERS;
    JAVA_CLASS_META_PARAMETERS;
    JAVA_CLASS_PARAMETER;
    JAVA_CLASS_PARAMETERS;
    LOOP;
    LOOP_AND;
    LOOP_OR;
    LOOP_TIMES;
    LOOP_PLUS;
    LOOP_PRS_AND;
    LOOP_PRS_OR;
    LOOP_XOR;
    NAMED_ENV;
    NEW_JAVA;
    NEW_JAVA_CLASS;
    PORT_LIST;
    PRS_AND;
    PRS_NOT;
    PRS_OR;
    PRS_EXPRESSION;
    COSIM_EXPRESSION;
    RANGE;
    REFINEMENT;
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
    // flags used by variableDeclaration[] to indicate where variable
    // is declared.  Modified from CastTwoTree.g; should share later.
    private static final int PORT_PARAM        = 0;
    private static final int META_PARAM        = 1;
    private static final int IMPLIED_PORT_PARAM = 6;
    private static final int ENV_EXTRA_PARAM   = 7;
    // These three are expanded out of LOCAL_VAR.  TOP_LEVEL is used
    // for all variable declaration statements not in one of the two
    // blocks.
    private static final int TOP_LEVEL         = 2;
    private static final int IN_PRS_BLOCK      = 3;
    private static final int IN_SUBCELLS_BLOCK = 4;
    private static final int IN_ALIAS_BLOCK = 5;

    private boolean verbose = false;

    /**
     * To enable more sophisticated control of syntax error handling
     * than commenting out the body of syntaxError().
     **/
    public void setVerbosity(boolean verbose) {
        this.verbose = verbose;
    }

    private antlr.TokenStreamSelector selector = null;

    public void setSelector(final antlr.TokenStreamSelector selector) {
        this.selector = selector;
    }

    private void getDumbBlockString(final com.avlsi.cast.impl.ASTWithInfo ast,
                                    final Token token)
    throws RecognitionException, TokenStreamException {
        ast.setText(CastTwoUtil.getDumbBlockString(selector, this));
        ast.setLine(token.getLine());
        ast.setColumn(token.getColumn() + 1);
    }

    static CastTwoParserInterface getParser(Class parser, String s, int lineNum,
                                            int colNum, String fileName) {
        final antlr.TokenStreamSelector
            selector = CastTwoUtil.getSelector(s, lineNum, colNum, fileName);
        final Class[] formals = new Class[] { antlr.TokenStream.class };
        final Object[] actuals = new Object[] { selector };
        final CastTwoParserInterface result;

        try {
            result = (CastTwoParserInterface) parser.getConstructor(formals)
                                                    .newInstance(actuals);
        } catch (Exception e) {
            throw new RuntimeException("Valid constructor not found for " +
                                       parser.getName());
        }

        result.setSelector(selector);
        result.setFilename(fileName);
        // Use fully qualified class name so we don't have to add import lines
        // to each subparser's grammar
        result.setASTNodeClass(com.avlsi.cast.impl.ASTWithInfo.class.getName());
        return result;
    }

    public static CastTwoParser getParser(String s, int lineNum, int colNum,
                                          String fileName) {
        return (CastTwoParser) getParser(CastTwoParser.class, s, lineNum,
                                         colNum, fileName);
    }

    private String getLineNumber() {
        try {
            return String.valueOf(LT(1).getLine());
        } catch (TokenStreamException e) {
            return "unknown line";
        }
    }

    /**
     * All syntax warning messages about old cast syntax that's no longer
     * allowed go through here.  Controlled by verbose.
     **/
    private void syntaxError(String errorMsg) {
        if (verbose) {
            System.err.println("SYNTAX: " + getFilename() + ": " +
                               getLineNumber() + ": " + errorMsg);
        }
    }

    private void warn(final String message) {
        System.err.println("Warning: " + getFilename() + ": " +
                           getLineNumber() + ": " + message);
    }

    /**
     * Like syntaxError(), but stops parsing, even if verbose is off.
     **/
    private void fatalError(String errorMsg) {
        throw new AssertionError("fatal error in " + getFilename() + " at " +
                    getLineNumber() + ": " + errorMsg);
    }
}

goal 
    {
        if (selector == null) {
            // XXX: throw exception
            System.err.println("CastTwo selector not set");
            System.exit(1);
        }
    }
    : compilationUnit EOF
    ;

compilationUnit 
    : moduleDeclaration
      ( importDeclaration )*
      ( fileLevelBodyStatement )*
    ;

moduleDeclaration
    : MODULE^ moduleIdent SEMI!
    | /* empty */
      { #moduleDeclaration = #( [MODULE], [IDENT, ""] );
        syntaxError("No module declaration");
      }
    ;

importDeclaration 
    : IMPORT^ importIdent SEMI!
    ;

moduleIdent!
    { StringBuffer accum = new StringBuffer(); }
    : id:IDENT { accum.append(id.getText()); }
      ( moduleMetaParamList[accum] )?
      ( di:DOT_IDENT { accum.append("." + di.getText()); }
        ( moduleMetaParamList[accum] )? )*
    {
        #moduleIdent = #( [IDENT, accum.toString()] ); 
        #moduleIdent.copyInfo(#id);
    }
    ;

moduleMetaParamList![StringBuffer accum]
    : LPAREN {accum.append('(');}
      moduleMetaParams[accum]
      RPAREN {accum.append(')');}
    ;

moduleMetaParams![StringBuffer accum]
    : moduleMetaParam[accum]
      ( COMMA {accum.append(',');} moduleMetaParam[accum] )*
    ;
    
moduleMetaParam![StringBuffer accum]
    { boolean negP = false; }
    : ( MINUS {negP = true;} )? i:integer
      {
          // Don't just append '-' because we want -0 to be 0
          java.math.BigInteger v = CastTwoUtil.bigIntegerValue(#i.getText());
          if (negP)
              v = v.negate();
          accum.append(v.toString());
      }
    | TRUE {accum.append("true");}
    | FALSE {accum.append("false");}
    | LCURLY {accum.append('{');}
      moduleMetaParams[accum]
      RCURLY {accum.append('}');}
    ;

// module.cellname (no meta params)
// ident ( (metas)? . ident )*
cellTypeIdent!
    { StringBuffer accum = new StringBuffer(); }
    : id:IDENT { accum.append(id.getText()); }
      cellTypeTail[accum]
    {
        #cellTypeIdent = #( [IDENT, accum.toString()] ); 
        #cellTypeIdent.copyInfo(#id);
    }
    ;

cellTypeTail![StringBuffer accum]
    : ( ( moduleMetaParamList[null] )? DOT_IDENT ) =>
      ( moduleMetaParamList[accum] )?
      di:DOT_IDENT {accum.append('.' + di.getText());}
      cellTypeTail[accum]
    | /*empty*/
    ;

dottedIdent
    { StringBuffer accum = new StringBuffer(); }
    : id:IDENT! { accum.append(id.getText()); }
      ( di:DOT_IDENT! { accum.append("." + di.getText()); } )+
    {
        #dottedIdent = #( [IDENT, accum.toString()] ); 
        #dottedIdent.copyInfo(#id);
    }
    ;

// module ( . module )* . ( cell | * )
// ident (metas)? ( . ident (metas)? )* . ( ident | * )
importIdent
    { StringBuffer accum = new StringBuffer(); }
    : id:IDENT^ { accum.append(id.getText()); }
      ( moduleMetaParamList[accum] )?
      ( di:DOT_IDENT {accum.append('.' + di.getText());}
        ( moduleMetaParamList[accum] )? )*
      ( di2:DOT_IDENT {accum.append('.' + di2.getText());}
      | DOT TIMES {accum.append(".*");} )
    { #id.setText(accum.toString()); #id.setType(IMPORT_IDENT); }
    ;

bodyStatements[boolean fileLevelP]
    : ( { fileLevelP }? ( fileLevelBodyStatement )*
        | { ! fileLevelP }? ( internalBodyStatement[false] )*
      )
    { #bodyStatements = #( [BODY_STATEMENT_LIST], #bodyStatements ); }
    ;

// statements that can be at the top level of a file (or inside
// loops/ifs at the top level of a file)
fileLevelBodyStatement
    : typeDeclaration
    | channelDeclaration
    | aliasDeclaration
    | typeBody[true]
    | (assignableExpression ASSIGN) => assignmentStatement
    | variableDeclarationStatement[TOP_LEVEL]
    | loopStatement[true]
    | ifStatement[true]
    | prsBlock
      { syntaxError("prs block outside of cell definition"); }
    ;

internalBodyStatement[boolean isEnv]
    // lookahead is used here to avoid having keywords for level / csp / ...
    : ( ( FRAGMENT )? PRS ) => prsBlock
    | cspBlock
    | subcellsBlock
    | { !isEnv }? envBlock
    | javaBlock
    | netlistBlock
    | directiveBlock
    | typeBody[false]
    | ( topLevelConstantType ) => topLevelConstantDeclaration
    | (assignableExpression ASSIGN) => assignmentStatement
        // XXX: Allowing assignmentStatements at the top level for the
        // sake of top-level constant assignments (like "int d[0..2];
        // d[1] = 4;").  This will let assignments of ports to each
        // other slip through without warning.
    | variableDeclarationStatement[TOP_LEVEL]
    | loopStatement[false]
    | ifStatement[false]
    | subtypesBlock
    | verilogBlock
    ;

channelDeclaration
    : DEFCHAN^
      name:IDENT
      ( metaParameterList ( portParameterList )? )?
      ( inheritanceList[false] )?
      ( ref:refinement[false] )?
      channelBody
    ;

channelBody
    : LCURLY! channelStatements RCURLY!
    ;

channelStatements
    : ( channelStatement )*
    { #channelStatements = #( [BODY_STATEMENT_LIST], #channelStatements ); }
    ;

channelStatement
    : aliasBlock
    | assertBlock
    | directiveBlock
    ;

// Packages up a string; CastTwoTreeParser will deal with it.
aliasBlock
    : alias:ALIAS^ l:LCURLY!
    {
        getDumbBlockString(#alias, l);
    }
    ;

assertBlock
    : assertion:ASSERT^ l:LCURLY!
    {
        getDumbBlockString(#assertion, l);
    }
    ;

typeDeclaration
    : typeHeader typeBody[false]
    { #typeDeclaration = #( [TYPE_DEFINITION], #typeDeclaration ); }
    ;

typeHeader 
    : DEFINE! id:IDENT
      ( metaParameterList ( portParameterList ( impliedParameterList )? )? )?
      inheritanceAndRefinement[false]
    ;

aliasDeclaration
    : DEFALIAS^ id:IDENT ( metaParameterList )? refinement[false] SEMI!
    ;

inheritanceAndRefinement[boolean inEnv]
    : ATTRIBUTES ( inheritanceList[inEnv] )?
    | ( inheritanceList[inEnv] )? ( refinement[inEnv] )?
    ;

inheritanceList[boolean inEnv]
    : inheritance[inEnv] ( inheritance[inEnv] )*
    { #inheritanceList = #( [INHERITANCE_LIST], #inheritanceList ); }
    ;

inheritance[boolean inEnv]
    : l:LT^ { #l.setType(INHERITANCE); } PLUS! cellTypeIdent
      // metaparameters
      ( (LPAREN expressionList RPAREN) => typeParameterList 
        | /* empty */ )
      ( { inEnv }? COLON! cellTypeIdent
        // env metaparameters
        ( (LPAREN expressionList RPAREN) => typeParameterList 
        | /* empty */ )
      )?
    ;

refinement[boolean inEnv]
    : l:LT^ { #l.setType(REFINEMENT); } COLON! cellTypeIdent
      // metaparameters
      ( (LPAREN expressionList RPAREN) => typeParameterList 
        | /* empty */ )
      ( { inEnv }? COLON! cellTypeIdent
        // env metaparameters
        ( (LPAREN expressionList RPAREN) => typeParameterList 
        | /* empty */ )
      )?
    ;

metaParameterList
    : LPAREN! parameterList[META_PARAM] RPAREN!
    ;
    
portParameterList
    : LPAREN! parameterList[PORT_PARAM] RPAREN!
    ;

impliedParameterList
    : LPAREN! parameterList[IMPLIED_PORT_PARAM] RPAREN!
    ;

envExtraParameterList
    : LPAREN! parameterList[ENV_EXTRA_PARAM] RPAREN!
    ;

// Declaration of ports of a cell.
parameterList[int declKind]
    : ( v:variableDeclaration[declKind]
        ( options { warnWhenFollowAmbig = false; } :
          SEMI! variableDeclaration[declKind]
        )*
        ( SEMI! )?
      )?
    { #parameterList = #( [FORMALS], #parameterList ); 
      if (#v != null)
          #parameterList.copyInfo(#v); }
    ;

typeBody[boolean fileLevelP]
    : lc:LCURLY^ {#lc.setType(BLOCK);} bodyStatements[fileLevelP] RCURLY!
    ;

// Shares a fair amount of code with variableDeclarationStatement and
// its children, but not enough to make it worth working another flag
// through everything
topLevelConstantDeclaration!
    : t:topLevelConstantType id:IDENT
      ( a:arraySelector )? ( ASSIGN assign:expression )?
      SEMI
      {
          final com.avlsi.cast.impl.ASTWithInfo assignmentBit;
          if (#assign == null)
              assignmentBit = null;
          else
              assignmentBit = #( [ASSIGN], #assign );

          // nulls are inline/flatten.
          #topLevelConstantDeclaration =
              #( [VAR_DECL], #id,
                 null, null, 
                 #( [TYPE], #t, #a, #([CHANNEL_WIDTH]) ),
                 assignmentBit
               );
      }
    ;

topLevelConstantType
    : INT
    | FLOAT
    | BOOL
    ;

instantiation
    : variableDeclaration[IN_SUBCELLS_BLOCK]
    ;

// declKind can only be one of the LOCAL_VAR types
variableDeclarationStatement[int declKind] 
    : variableDeclaration[declKind] SEMI!
    ;

variableDeclaration![int declKind]
    : ( i:INLINE { if (declKind != IN_SUBCELLS_BLOCK) fatalError("cells may only be inlined in subcells blocks"); }
        | f:FLATTEN
          { warn("flatten is deprecated");
            if (declKind != IN_PRS_BLOCK)
                fatalError("cells may only be flattened in prs blocks");
          }    
      )? 
        t:type[declKind == IN_SUBCELLS_BLOCK || declKind == IN_PRS_BLOCK ||
               declKind == PORT_PARAM || declKind == IMPLIED_PORT_PARAM ||
               declKind == ENV_EXTRA_PARAM || declKind == IN_ALIAS_BLOCK ]
        c:channelWidth v:variableDeclarators[declKind, #t, #c, #i, #f]
    { #variableDeclaration = #v; }
    ;

variableDeclarators[int declKind, AST t, AST width, AST inlineAST,
                    AST flattenAST]
    : variableDeclarator[declKind, getASTFactory().dupTree(t),
                         getASTFactory().dupTree(width),
                         getASTFactory().dupTree(inlineAST),
                         getASTFactory().dupTree(flattenAST)]
        ( COMMA! variableDeclarator[declKind, getASTFactory().dupTree(t),
                                    getASTFactory().dupTree(width),
                                    getASTFactory().dupTree(inlineAST),
                                    getASTFactory().dupTree(flattenAST)] )*
    ;

variableDeclarator![int declKind, AST t, AST width, AST inlineAST,
                    AST flattenAST]
    { final boolean formalP =
          declKind == PORT_PARAM || declKind == IMPLIED_PORT_PARAM ||
          declKind == ENV_EXTRA_PARAM;
      boolean p = false, m = false; }
    : ( {formalP}? ( p1:PLUS  {p=true;} ( m1:MINUS {m=true;} )?
                   | m2:MINUS {m=true;} ( p2:PLUS  {p=true;} )? ) )?
      id:IDENT ( a:arraySelector )?
      ( { declKind == IMPLIED_PORT_PARAM }? c:COLON id2:IDENT )?
      ( { declKind == ENV_EXTRA_PARAM }? COLON ae:assignableExpression )?
      ( { !formalP }? vi:variableInitializer )?
    { // REVIEW: this is really ugly!!!  is there a better way?
      if (! formalP && (p || m))
          syntaxError("shouldn't have directionality on metaparameters");
      if (p && m)
          #variableDeclarator
            = #( [VAR_DECL], id, [PLUS], [MINUS], inlineAST,
                 flattenAST, #([TYPE], t, a, width), c, id2, ae, vi );
      else if (p)
          #variableDeclarator
            = #( [VAR_DECL], id, [PLUS], inlineAST,
                 flattenAST, #([TYPE], t, a, width), c, id2, ae, vi );
      else if (m)
          #variableDeclarator
            = #( [VAR_DECL], id, [MINUS], inlineAST,
                 flattenAST, #([TYPE], t, a, width), c, id2, ae, vi );
      else {
          if (formalP) syntaxError("no directionality declared on port");
          #variableDeclarator
            = #( [VAR_DECL], id, inlineAST,
                 flattenAST, #([TYPE], t, a, width), c, id2, ae, vi );
      }
    }
    ;


variableInitializer
    : lp:LPAREN^ {#lp.setType(PORT_LIST);} expressionList RPAREN!  // normal ports
      ( LPAREN! expressionList RPAREN! ) ? // implied
    | ASSIGN^ expression
    ;

arraySelector
    : lb:LBRACK^ {#lb.setType(ARRAY_SUB);} ranges RBRACK!
    ;

ranges 
    : range ( COMMA! range )*
    ;

// For external access from the CDL parser
startRange
    : r:range EOF!
    {
        #startRange = #r;
    }
    ;

range
    : expression ( DOTDOT! expression )?
    { #range = #( [RANGE], range ); }
    ;

cellType
    : cellTypeIdent typeParameterList EOF!
    { #cellType = #( [USER_TYPE], #cellType ); }
    ;

type[boolean nonprimitiveAllowed]
    : primitiveType
    | cellTypeIdent {
        if (!nonprimitiveAllowed)
            syntaxError("Non-primitive declarations allowed inside PRS and subcells block only.");
      }
      typeParameterList
      { #type = #( [USER_TYPE], #type ); }
    ;

channelWidth
    : l:LBRACK^ { #l.setType(CHANNEL_WIDTH); } expression RBRACK!
    | /* empty */ { #channelWidth = #( [CHANNEL_WIDTH] ); }
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

loopStatement[boolean fileLevelP]
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT COLON! range COLON! bodyStatements[fileLevelP] GT!
    ;

ifStatement[boolean fileLevelP]
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! bodyStatements[fileLevelP] RBRACK!
    ;

assignmentStatement 
    : assignment SEMI!
    ;

assignment 
    : assignableExpression assignmentRhs
    { #assignment = #( [ASSIGN], #assignment ); }
    ;

assignmentRhs
    : ( ASSIGN! expression )+
    ;

// fix me up later!
// ugh, duplication
assignableExpression
    : id:IDENT
      (! i:DOT_IDENT
         { final String s = i.getText();
           #assignableExpression = #( [FIELD_ACCESS],
                                          #assignableExpression,
                                          #[ FIELD_IDENT, s ] );
           #assignableExpression.copyInfo(#i);
         }
      |! as:arraySelector
         { #assignableExpression = #( [ARRAY_ACCESS],
                                      #assignableExpression,
                                      #as
                                    );
           #assignableExpression.copyInfo(#as);
         }
      )*
    ;

expressionList
    : ( /* empty */ { #expressionList = #( [EXPRESSION_LIST] ); }
      | e:expression
        ( options { warnWhenFollowAmbig = false; } : COMMA! expression )*
        ( COMMA! )?
        { #expressionList = #( [EXPRESSION_LIST], #expressionList);
          #expressionList.copyInfo(#e);
        }
      )
    // commented out version causes non-determinism, above should
    // be equivalent
    //: ( expression ( COMMA! expression )* )?
    ;

startExpressionList
    : expressionList EOF!
    ;

arrayComprehension
    : e:arrayComprehensionItem ( COMMA! arrayComprehensionItem )*
        { #arrayComprehension = #( [EXPRESSION_LIST], #arrayComprehension);
          #arrayComprehension.copyInfo(#e);
        }
    ;

arrayComprehensionItem
    : ( expression
      | LT_COMMA! IDENT COLON! range COLON! arrayComprehension GT!
      )
    ;

startExpression
    : expr:expression EOF!
    {
        #startExpression = #expr;
    }
    ;

expression 
    : co:conditionalInclusiveOrExpression
    {
        #expression = #( [EXPRESSION], #expression);
        #expression.copyInfo(#co);
    }
    ;

conditionalInclusiveOrExpression 
    : conditionalExclusiveOrExpression ( OR^ conditionalExclusiveOrExpression )*
    ;

conditionalExclusiveOrExpression 
    : conditionalAndExpression ( XOR^ conditionalAndExpression )*
    ;

conditionalAndExpression 
    : relationalExpression ( AND^ relationalExpression )*
    ;

relationalExpression 
    : additiveExpression 
      ( options { greedy = true; } : // turn off warning from arrayComprehension
        ( EQ^ | NE^ | LT^ | LE^ | GT^ | GE^ ) additiveExpression )*
    ;

additiveExpression 
    : multiplicativeExpression ( ( PLUS^ | MINUS^ ) multiplicativeExpression )*
    ;

additiveExpr
    : ae:additiveExpression
    {
        #additiveExpr = #( [EXPRESSION], #additiveExpr);
        #additiveExpr.copyInfo(#ae);
    }
    ;

multiplicativeExpression 
    : unaryExpression ( ( TIMES^ | DIV^ | MOD^ ) unaryExpression )*
    ;

unaryExpression 
    : PLUS^ {#PLUS.setType(UNARY_PLUS);} unaryExpression
    | MINUS^ {#MINUS.setType(UNARY_MINUS);} unaryExpression
    | NOT^ unaryExpression
    | exponentialExpression
    ;

exponentialExpression
    : selectionExpression ( EXP^ exponentialExpression )?
    ;

// treeify me
// ugh, duplication
selectionExpression
    : primaryExpression
      (! i:DOT_IDENT
         { final String s = i.getText();
           #selectionExpression =
               #( [FIELD_ACCESS], #selectionExpression, #[ FIELD_IDENT, s ] );
           #selectionExpression.copyInfo(#i);
         }
      |! as:arraySelector
         { #selectionExpression =
               #( [ARRAY_ACCESS], #selectionExpression, #as );
           #selectionExpression.copyInfo(#as);
         }
      )*
    ;

integer
    : NUM_INT | NUM_HEX
    ;

primaryExpression
    : (IDENT LPAREN) => IDENT LPAREN! expressionList RPAREN!
    | IDENT
    | integer
    | NUM_REAL
    | TRUE
    | FALSE
    | anonymousArray
    | s:QuotedString {
        #s.setType(NUM_INT);
        #s.setText(com.avlsi.util.math.BigIntegerUtil.fromASCII(#s.getText()).toString());
      }
    | LPAREN! conditionalInclusiveOrExpression RPAREN!
    | primitiveType LPAREN! coe:conditionalInclusiveOrExpression! RPAREN! {
        #primaryExpression = #( #primaryExpression, #coe );
      }
    | loop:LT^ ( PLUS! { #loop.setType(LOOP_PLUS); }
               | TIMES! { #loop.setType(LOOP_TIMES); }
               | XOR! { #loop.setType(LOOP_XOR); } )
      IDENT COLON! range COLON! additiveExpr GT!
    | loop_or:LT_OR^ { #loop_or.setType(LOOP_OR); }
      IDENT COLON! range COLON! additiveExpr GT!
    | loop_and:LT_AND^ { #loop_and.setType(LOOP_AND); }
      IDENT COLON! range COLON! additiveExpr GT!
    ;

anonymousArray 
    : lc:LCURLY^ {#lc.setType(ARRAY);} arrayComprehension RCURLY!
    ;

//
// env stuff.  Lots of syntactic sugar with curly-brace position
//

envBlock
    : ENV^ (( LCURLY! ( namedEnv )* RCURLY! )
            | namedEnv )
    ;

namedEnv
    : name:IDENT ( metaParameterList ( envExtraParameterList )? )?
      inheritanceAndRefinement[true]
    (( LCURLY! envBlockStatements RCURLY! )
     | singleEnvBlockStatement )
    { #namedEnv = #( [NAMED_ENV], #( [TYPE_DEFINITION], #namedEnv ) ); }
    ;

envBlockStatements
    : ( internalBodyStatement[true] )*
    { #envBlockStatements = #( [BLOCK], #( [BODY_STATEMENT_LIST], #envBlockStatements ) ); }
    ;

singleEnvBlockStatement
    : envBlockStatement
    { #singleEnvBlockStatement = #( [BLOCK], #( [BODY_STATEMENT_LIST], #singleEnvBlockStatement ) ); }
    ;

// Subset of bodyStatement
envBlockStatement
    : ( ( FRAGMENT )? PRS ) => prsBlock
    | subcellsBlock
    | subtypesBlock
    | directiveBlock
    | cspBlock
    | netlistBlock
    | javaBlock
    | verilogBlock
    ;

//
// PRS stuff now done as a string passed to a separate parser.  No
// lazy evaluation yet.
//

prsBlock
    : ( f:FRAGMENT )? p:PRS^ l:LCURLY!
    {
        getDumbBlockString(#p, l);
    }
    ;

// For external use by PrsCallback
startPrsNodeExpression!
    : node:prsNodeExpression EOF!
    {
        #startPrsNodeExpression = #node;
    }
    ;

prsNodeExpression
//ugh, duplication
    : IDENT
      (! id:DOT_IDENT
         { final String s = id.getText();
           #prsNodeExpression
               = #( [FIELD_ACCESS],
                   #prsNodeExpression, #[ FIELD_IDENT, s ] ); 
           #prsNodeExpression.copyInfo(#id);
         }
      |! as:arraySelector
         { #prsNodeExpression
             = #( [ARRAY_ACCESS], #prsNodeExpression, #as ); 
           #prsNodeExpression.copyInfo(#as);
         }
      )*
    ;

//
// Contains the csp for the cell
//
cspBlock
    : csp:CSP^ ( cspPorts )? l:LCURLY!
    {
        getDumbBlockString(#csp, l);
    }
    ;

//
// A csp block can have ports explicitly specified to limit its
// creation of NodeChannels.
//
cspPorts
    : l:LPAREN^ { #l.setType(CSP_PORTS); }
      cspPort ( COMMA! cspPort )* RPAREN!
    ;

// Array access currently doesn't and might never work, so it's not accepted
cspPort
    : id:IDENT^
    ;

//
// Contains directives which are contained within other blocks.
//
directiveBlock
    : directive:DIRECTIVE^ l:LCURLY!
    {
        getDumbBlockString(#directive, l);
    }
    ;

//
// Contains a CDL body.  FIXME: A CDL body should be mutually exclusive with
// a subcell body.
//
netlistBlock
    : netlist:NETLIST^ l:LCURLY!
    {
        getDumbBlockString(#netlist, l);
    }
    ;

//
// Contains the non-flattenend subcells.  Conflicts with the prs block
// handled by the tree parser.  FIXME: permits internal node definition
//
// XXXXX
subcellsBlock
    : ( i:INLINE | f:FRAGMENT )? subcells:SUBCELLS^ l:LCURLY!
    {
        getDumbBlockString(#subcells, l);
    }
    ;

//
// Contains the definition of how subcells are refined in a refinement
//
subtypesBlock
    : subtypes:SUBTYPES^ l:LCURLY!
    {
        getDumbBlockString(#subtypes, l);
    }
    ;

// a Java block to specify co-sim parameters
javaBlock
    : j:JAVA^ LCURLY!
      ( oldJavaStatements 
        | newJavaStatements { #j.setType(NEW_JAVA); }
      ) RCURLY!
    ;

//
// new java syntax handling
//

newJavaStatements
    : ( newJavaStatement )+
    { #newJavaStatements = #( [BODY_STATEMENT_LIST], #newJavaStatements ); }
    ;

newJavaStatement
    : newJavaClassStatement
    | javaChannelStatement
    | directiveBlock
    ;

newJavaClassStatement
    : ( id:IDENT
        | d:dottedIdent { #id = #d; }
      )
      ( javaClassMetaParameters )?
      instanceName:IDENT // May be anonymous
      ( javaClassParameters )?
      ( javaClassInitializers )?
      SEMI!
    {
        #newJavaClassStatement = #( [NEW_JAVA_CLASS], #newJavaClassStatement );
        #newJavaClassStatement.copyInfo(#id);
    }
    ;

javaClassMetaParameters
    : l:LPAREN^ javaClassMetaParameter
                ( COMMA! javaClassMetaParameter )*
      RPAREN!
    { #l.setType(JAVA_CLASS_META_PARAMETERS); }
    ;

javaClassMetaParameter
    : expression
    ;

// channel/node order not enforced until tree-parsing.
javaClassParameters
    : l:LPAREN^ javaClassParameter ( COMMA! javaClassParameter )* RPAREN!
    { #l.setType(JAVA_CLASS_PARAMETERS); }
    ;

// Includes nodes and channels.
javaClassParameter
    : ( selectionExpression )
      ( javaChannelTimingInfo ) ?
      { #javaClassParameter = #( [JAVA_CLASS_PARAMETER], #javaClassParameter ); }
    ;

javaChannelTimingInfo
    : c:COLON^ { #c.setType(CHANNEL_TIMING_INFO); }
    ( integer
      | LPAREN! integer COMMA! integer COMMA! integer COMMA! integer
                COMMA! integer
        RPAREN!
    )
    ;

// These are the things which will be fed into enqueue() after the
// device in question has been start()ed.
javaClassInitializers
    : c:COLON^ { #c.setType(JAVA_CLASS_INITIALIZERS); }
      javaClassInitializer ( COMMA! javaClassInitializer )*
    ;

javaClassInitializer
    : TRUE
    | FALSE
    | integer
    | NUM_REAL
    | QuotedString
    ;

// Creates a BufferedChannel between java devices
javaChannelStatement
    : CHANNEL^ id:IDENT
      ( arraySelector )?
      ( javaChannelTimingInfo )?
      SEMI!
    ;

//
// old java syntax handling
//

oldJavaStatements
    : ( oldJavaStatement )+
    { #oldJavaStatements = #( [BODY_STATEMENT_LIST], #oldJavaStatements ); }
    ;

oldJavaStatement
    : oldJavaClassStatement
    | inputChannelStatement
    | outputChannelStatement
    | internalChannelStatement
    ;

oldJavaClassStatement
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
// Verilog block
//
verilogBlock
    : VERILOG^ ( ( LCURLY! ( verilogNamed )* RCURLY! ) | verilogNamed )
    ;

verilogNamed
    : name:IDENT LCURLY! verilogStatements RCURLY!
    ;

verilogStatements
    : ( verilogStatement )*
      { #verilogStatements = #( [BODY_STATEMENT_LIST], #verilogStatements ); }
    ;

verilogStatement
    : verilogIdent ( POUND! LPAREN! expressionList RPAREN! )?
      LPAREN! ( verilogParameters )? RPAREN! ( COLON! verilogFiles )? SEMI
    | COLON! verilogFiles SEMI
    | directiveBlock
    ;

verilogIdent
    : IDENT | dottedIdent
    ;

verilogParameters
    : verilogImplicitParameter ( COMMA! verilogImplicitParameter )*
    | verilogExplicitParameter ( COMMA! verilogExplicitParameter )*
    ;

// mid style
verilogImplicitParameter
    : selectionExpression
    ;

// snovak style
verilogExplicitParameter
    : ( DOT_IDENT | DOT! ESCAPED_VERILOG_IDENT )
      LPAREN! ( selectionExpression )? RPAREN
      // RPAREN instead of RPAREN!, because it resolves non-determinism later
      // in the tree parser
    ;

verilogFiles
    : QuotedString ( COMMA! QuotedString )*
    ;

// Parse fully qualified cell name minus plus for partial extraction
partialExtraction
    : cellTypeIdent
      ( ( LPAREN expressionList RPAREN ) => typeParameterList | /* empty */ )
      ( ( PLUS | MINUS ) ( PLUS | MINUS )? assignableExpression )*
      ( COLON!
        ( cellTypeIdent
          (
            ( LPAREN expressionList RPAREN ) => typeParameterList
          | /* empty */
          )
        | TIMES
        )
      )?
      EOF!
    ;

// Parse alint scenarios
alintFanin
    : node:prsNodeExpression
      ( ASSIGN! s:NUM_INT { s.getText().equals("0") ||
                            s.getText().equals("1") }?
      | PLUS
      | MINUS )
      { #alintFanin = #( [EXPRESSION], #( [ALINT_FANIN], #alintFanin ) );
        #alintFanin.copyInfo(#node);
      }
    ;

alintScenario
    : e:alintScenarioItem ( COMMA! alintScenarioItem )*
        { #alintScenario = #( [EXPRESSION_LIST], #alintScenario );
          #alintScenario.copyInfo(#e);
        }
    ;

alintScenarioItem
    : ( alintFanin
      | LT_COMMA! IDENT COLON! range COLON! alintScenario GT!
      )
    ;

startAlintScenario!
    : LCURLY! ( scenario:alintScenario )? RCURLY! EOF!
    {
        #startAlintScenario = #scenario;
    }
    ;

//---------------------------------------------------------------------------
// The CastTwo scanner
//---------------------------------------------------------------------------

class CastTwoLexer extends Lexer;
options {
    classHeaderSuffix = com.avlsi.cast.impl.CastLexerInterface;
    charVocabulary = '\0'..'\377';
    exportVocab = CastTwo;   // call the vocabulary "CastTwo"
    testLiterals = false;   // do not automatically test for literals
    k = 2;                  // two characters of lookahead
    caseSensitive = true;
    caseSensitiveLiterals = true;
    defaultErrorHandler = false;
}

tokens {
    ATTRIBUTES      = "attributes"      ;
    BOOL            = "bool"            ;
    DEFINE          = "define"          ;
    FLOAT           = "float"           ;
    FLATTEN         = "flatten"         ;
    IMPORT          = "import"          ;
    INLINE          = "inline"          ;
    INT             = "int"             ;
    MODULE          = "module"          ;
    NODE            = "node"            ;
    TRUE            = "true"            ;
    FALSE           = "false"           ;
    ENV             = "env"             ;
    FRAGMENT        = "fragment"        ;
    // channels
    DEFCHAN         = "defchan"         ;
    ALIAS           = "alias"           ;
    ASSERT          = "assert"          ;
    // prs
    PRS             = "prs"             ;
    TIMED           = "timed"           ;
    AFTER           = "after"           ;
    AFTER_PS        = "after_ps"        ;
    UNSTAB          = "unstab"          ;
    METASTABLE      = "metastab"        ;
    ISOCHRONIC      = "isochronic"      ;
    // subcells
    SUBCELLS        = "subcells"        ;
    SUBTYPES        = "subtypes"        ;
    // directive
    DIRECTIVE       = "directives"      ;
    // netlist
    NETLIST         = "netlist"         ;
    // csp
    CSP             = "csp"             ;
    // java
    CHANNEL         = "Channel"         ;
    JAVA            = "java"            ;
    JAVACLASS       = "javaclass"       ;
    INPUTCHAN       = "inputchannel"    ;
    OUTPUTCHAN      = "outputchannel"   ;
    INTERNALCHAN    = "internalchannel" ;
    // spec
    EXCLHI          = "exclhi"          ;
    EXCLLO          = "excllo"          ;
    EXCLCC          = "exclcc"          ;
    NOCC            = "nocc"            ;
    // verilog
    VERILOG         = "verilog"         ;
    // aliased types
    DEFALIAS        = "defalias"        ;
}

{
    public Token makeToken(final int t) {
        final Token tok = super.makeToken(t);
        ((com.avlsi.cast.impl.TokenWithInfo) tok).setFilename(getFilename());
        return tok;
    }
}

//---------------------------------------------------------------------------
// OPERATORS
//---------------------------------------------------------------------------

ARROW           : "->"  ;
EXP             : "**"  ;
PLUS            : '+'   ;
MINUS           : '-'   ;
TIMES           : '*'   ;
DIV             : '/'   ;
MOD             : '%'   ;
AND             : '&'   ;
OR              : '|'   ;
XOR             : '^'   ;
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
DOT             : '.'   ;
DOTDOT          : ".."  ;
POUND           : "#"   ;
CELEM_ARROW     : "#>"  ;
COMB_ARROW      : "=>"  ;
LT_AND          : "<&"  ;
LT_OR           : "<|"  ;
LT_COMMA        : "<,"  ;

WS
    : ( ' ' | '\t' | '\f' | ( '\n' { newline(); } ) | '\r' )
    { $setType(Token.SKIP); }
    ;

COMMENT_1
        : "/*"
           ( options { generateAmbigWarnings=false; }
           :    { LA(2) != '/' }? '*'
           |    '\n'            {newline();}
           |   ~('*' | '\n')
           )*
          "*/"
        {$setType(Token.SKIP);}
    ;

COMMENT_2
    : "//" (~ '\n' )* '\n'
      { $setType(Token.SKIP); newline(); }
    ;

IDENT
    : id:REAL_IDENT { $setType(id.getType()); }
    | ESCAPED_IDENT ;

// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
protected REAL_IDENT
    options {testLiterals=true;}
    : ( LETTER | '_' ) ( LETTER | '_' | DIGIT )*
      { $setType(IDENT); }
    ;

protected ESCAPED_IDENT
    : '"' ( LETTER | DIGIT | '_' | '!' )+ '"'
      { 
          final String s = $getText;
          final String ss = s.substring(1, s.length() - 1);
          $setText(ss);
      }
    ;

// a numeric literal or an identifier
NUM_INT
    { boolean identP = false; }
    // Hexadecimal case
    :   { LA(1) == '0' && LA(2) == 'x' }?
        DIGIT! LETTER! ( DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' |
                                 'a' | 'b' | 'c' | 'd' | 'e' | 'f' )+
        { $setType(NUM_HEX); }
    // Normal number/identifier case
    |   ( DIGIT )+ ( IDENT { $setType(IDENT); identP = true; } )?
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

ESCAPED_VERILOG_IDENT
    : '\\'! (~(' ' | '\t' | '\n' | '\r'))+
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
