//
// Copyright 2001 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

//
// quick CAST Parser Grammar (for use with antlr http://www.antlr.org/)

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
    // You want to modify CastInfo.g instead.
    //
    // Warning: does not get all information and will fail on valid cast
    // files with complicated channels.
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;
}

{
    import java.io.*;
    import antlr.TokenStreamSelector;
    import com.avlsi.cast.impl.ASTWithInfo;
    import com.avlsi.cast.impl.CastParserInterface;
    import com.avlsi.cast.impl.TokenWithInfo;
}

class CastInfoParser extends CastTwoParser;

importDeclaration!
    : IMPORT
      ( IDENT { syntaxError("Using old import syntax"); }
        | importIdent
      )
      SEMI!
    ;

subcellsBlock!
    : SUBCELLS LCURLY
    {
        getDumbBlockString();
    }
    ;
