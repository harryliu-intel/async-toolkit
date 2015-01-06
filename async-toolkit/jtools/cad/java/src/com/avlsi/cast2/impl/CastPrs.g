//
// Copyright 2000 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

//
// CAST Sub-Parser Grammar for PRS blocks (for use with antlr
// http://www.antlr.org/)
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
    // You want to modify CastPrs.g instead.
    //
    // FIXME: doesn't syntax-check well
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;
}

// Import the necessary classes
{
}

//---------------------------------------------------------------------------
// Define a Parser, calling it CastPrsParser
//---------------------------------------------------------------------------
class CastPrsParser extends CastTwoParser;
options {
}

goal 
    : prsInternalBlock EOF
    ;

// Doesn't have surrounding "prs {" and "}" -- those were removed at
// the cell level
prsInternalBlock
    : prsStatements
    ;

prsStatements
    : ( prsStatement )*
    { #prsStatements = #( [BODY_STATEMENT_LIST], #prsStatements ); }
    ;

prsStatement
    : prsIfStatement
    | prsLoopStatement
    | (prsModifiers prsExpression ( ARROW | CELEM_ARROW | COMB_ARROW )) =>
          prsAction
    | directiveBlock
    | (assignableExpression ASSIGN) => assignmentStatement
    | variableDeclarationStatement[IN_PRS_BLOCK]
    | assertBlock
    ;

prsIfStatement
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! prsStatements RBRACK!
    ;

prsLoopStatement 
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT
       COLON! range COLON! prsStatements GT!
    ;

startPrsAction
    : prs:prsAction EOF!  { #startPrsAction = #prs; }
    ;

prsAction
    : prsModifiers
      prsExpression ( ARROW^ | CELEM_ARROW^ | COMB_ARROW^ )
      prsNodeExpression ( PLUS | MINUS )
    ;

prsModifiers
    : ( ISOCHRONIC )? ( UNSTAB | METASTABLE )?
      ( TIMED ( LBRACK! expression ( COMMA! expression )? RBRACK! )? )?
      ( ( AFTER | AFTER_PS ) expression )? 
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
    : ( la:LT_AND^ {#la.setType(LOOP_PRS_AND);}
      | lo:LT_OR^  {#lo.setType(LOOP_PRS_OR); } )
      id:IDENT COLON! range COLON! prsExpression GT!
    | prsSelectionExpression
    ;

// review, this allows (foo | bar).baz[0], too general!
prsSelectionExpression
//ugh, duplication
    : prsPrimaryExpression
      (! id:DOT_IDENT
         { final String s = id.getText();
           #prsSelectionExpression
               = #( [FIELD_ACCESS],
                   #prsSelectionExpression, #[ FIELD_IDENT, s ] );
           #prsSelectionExpression.copyInfo(#id);
         }
      |! as:arraySelector
         { #prsSelectionExpression
             = #( [ARRAY_ACCESS], #prsSelectionExpression, #as );
           #prsSelectionExpression.copyInfo(#as);
         }
      )*
    ;

prsPrimaryExpression
    : IDENT
    | LPAREN! prsOrExpression RPAREN!
    ;

