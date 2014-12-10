//
// Copyright 2000 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

//
// CAST Sub-Parser Grammar for subcells blocks (for use with antlr
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
    // You want to modify CastSubcells.g instead.
    //
    // FIXME: doesn't syntax-check well
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;
}

// Import the necessary classes
{
}

//---------------------------------------------------------------------------
// Define a Parser, calling it CastSubcellsParser
//---------------------------------------------------------------------------
class CastSubcellsParser extends CastTwoParser;
options {
    classHeaderSuffix = CastTwoParserCallback;
}

goal
    : subcellStatements EOF
    ;

goalCallback[CastTwoUtil.ParserCallback cb]
    : subcellStatementsCallback[cb] EOF
    ;

// Doesn't have surrounding "subcells {" and "}" -- those were removed at
// the cell level
subcellStatements
    : ( subcellStatement )*
    { #subcellStatements = #( [BODY_STATEMENT_LIST], #subcellStatements ); }
    ;

subcellStatementsCallback[CastTwoUtil.ParserCallback cb]
    : ( stmt:subcellStatement! { cb.bodyStatement(#stmt); } )*
    ;

subcellStatement
    : (assignableExpression ASSIGN) => assignmentStatement
    | variableDeclarationStatement[IN_SUBCELLS_BLOCK]
    | subcellLoopStatement
    | subcellIfStatement
    | assertBlock
    | directiveBlock
    ;

// TODO: refactor so it shares with other loops
subcellLoopStatement
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT
       COLON! range COLON! subcellStatements GT!
    ;

// TODO: refactor so it shares with other if statements
subcellIfStatement
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! subcellStatements RBRACK!
    ;

