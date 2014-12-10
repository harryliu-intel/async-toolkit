/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import antlr.collections.AST;
import antlr.RecognitionException;
import antlr.TokenStreamException;
import antlr.TokenStreamSelector;
import antlr.LLkParser;

/**
 * Lets tools handle multiple versions of cast with specific code only
 * for constructors.
 **/
public interface CastParserInterface {
    void setSelector(final TokenStreamSelector selector);
    void goal() throws RecognitionException, TokenStreamException;
    void instantiation() throws RecognitionException, TokenStreamException;
    void setASTNodeClass(String nodeType);
    AST getAST();
    void setFilename(String f);
}
