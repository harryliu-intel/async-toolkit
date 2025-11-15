//
// Copyright 2002 Asynchronous Digital Design.  All rights reserved.
//
// $Id: //depot/sw/cad/java/main/src/com/avlsi/cast2/impl/CastSubcells.g#8 $
//

//
// CAST Sub-Parser Grammar for alias blocks (for use with antlr
// http://www.antlr.org/)


header {
    ///////////////////////////////////////////////////////////////////////
    //
    // Copyright 2002 Asynchronous Digital Design.  All rights reserved.
    //
    // Warning:  This file was AUTOMATICALLY GENERATED!!!
    // 
    // DO NOT check in.
    // DO NOT modify.
    //
    // You want to modify CastAliases.g instead.
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;
}

// Import the necessary classes
{
}

//---------------------------------------------------------------------------
// Define a Parser, calling it CastAliasesParser
//---------------------------------------------------------------------------
class CastAliasesParser extends CastTwoParser;
options {
}

goal 
    : aliasInternalBlock EOF
    ;

// Doesn't have surrounding "alias {" and "}" -- those were removed at
// the cell level
aliasInternalBlock
    : aliasStatements
    ;

aliasStatements
    : ( aliasStatement )*
    { #aliasStatements = #( [BODY_STATEMENT_LIST], #aliasStatements ); }
    ;

aliasStatement
    : variableDeclarationStatement[IN_ALIAS_BLOCK]
    | aliasLoopStatement
    | aliasIfStatement
    ;

// TODO: refactor so it shares with other loops
aliasLoopStatement
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT
       COLON! range COLON! aliasStatements GT!
    ;

// TODO: refactor so it shares with other if statements
aliasIfStatement
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! aliasStatements RBRACK!
    ;
