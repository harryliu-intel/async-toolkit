//
// Copyright 2002 Fulcrum Microsystems.  All rights reserved.
//
// $Id: $
//

//
// CAST Sub-Parser Grammar for assert blocks (for use with antlr
// http://www.antlr.org/)

header {
    ///////////////////////////////////////////////////////////////////////
    //
    // Copyright 2002 Fulcrum Microsystems.  All rights reserved.
    //
    // Warning:  This file was AUTOMATICALLY GENERATED!!!
    // 
    // DO NOT check in.
    // DO NOT modify.
    //
    // You want to modify CastAssert.g instead.
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;
}


// Import the necessary classes
{
}

//---------------------------------------------------------------------------
// Define a Parser, calling it CastAssertParser
//---------------------------------------------------------------------------
class CastAssertParser extends CastPrsParser;
options {
    importVocab = CastTwo;
}

goal 
    : assertInternalBlock EOF
    ;

// Doesn't have surrounding "assert {" and "}" -- those were removed at
// the cell level
assertInternalBlock
    : assertStatements
    ;

assertStatements
    : ( assertStatement )*
    { #assertStatements = #( [BODY_STATEMENT_LIST], #assertStatements ); }
    ;

assertStatement
    : assertPrsStatement
    | assertExclStatement
    | assertLoopStatement
    | assertIfStatement
    ;

assertPrsStatement
    : prsExpression SEMI!
    ;

assertExclStatement
    : ( EXCLHI^ | EXCLLO^ | EXCLCC^ | NOCC^ ) LPAREN! assertNodeList RPAREN! SEMI!
    ;

assertNodeList
    : prsNodeExpression ( COMMA! prsNodeExpression )*
    ;

// TODO: refactor so it shares with other loops
assertLoopStatement
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT
       COLON! range COLON! assertStatements GT!
    ;

// TODO: refactor so it shares with other if statements
assertIfStatement
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! assertStatements RBRACK!
    ;
