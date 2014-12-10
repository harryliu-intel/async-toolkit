//
// Copyright 2001 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

//
// CAST Sub-Parser Grammar for subtypes blocks (for use with antlr
// http://www.antlr.org/)

header {
    ///////////////////////////////////////////////////////////////////////
    //
    // Copyright 2001 Asynchronous Digital Design.  All rights reserved.
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
// Define a Parser, calling it CastSubtypesParser
//---------------------------------------------------------------------------
class CastSubtypesParser extends CastTwoParser;
options {
    classHeaderSuffix = CastTwoParserCallback;
}

goal
    : subtypesStatements EOF
    ;

goalCallback[CastTwoUtil.ParserCallback cb]
    : subtypesStatementsCallback[cb] EOF
    ;

// Doesn't have surrounding "subtypes {" and "}" -- those were removed at
// the cell level
subtypesStatements
    : ( subtypesStatement )*
    { #subtypesStatements = #( [BODY_STATEMENT_LIST], #subtypesStatements ); }
    ;

subtypesStatementsCallback[CastTwoUtil.ParserCallback cb]
    : ( stmt:subtypesStatement! { cb.bodyStatement(#stmt); } )*
    ;

subtypesStatement
    : subtypesRefinement 
    | subtypesLoopStatement
    | subtypesIfStatement
    | directiveBlock
    ;

subtypesRefinement!
    : ( inline:INLINE )?
      parent:type[true] COLON! GT! child:type[true] ae:assignableExpression SEMI!
    {
        #subtypesRefinement = #( [VAR_DECL], inline, ae,
                                #( [TYPE], parent, [CHANNEL_WIDTH] ),
                                #( [TYPE], child, [CHANNEL_WIDTH] ));
    }
    ;

subtypesLoopStatement
    : lt:LT^ {#lt.setType(LOOP);} id:IDENT
      COLON! range COLON! subtypesStatements GT!
    ;

subtypesIfStatement
    : lb:LBRACK^ {#lb.setType(IF);} expression ARROW! subtypesStatements RBRACK!
    ;

