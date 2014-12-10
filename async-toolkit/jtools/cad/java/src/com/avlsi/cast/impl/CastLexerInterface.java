/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import antlr.LexerSharedInputState;

/**
 * Lets tools handle multiple versions of cast with specific code only
 * for constructors.
 **/
public interface CastLexerInterface {
    LexerSharedInputState getInputState();
    void setTokenObjectClass(String cl);
    void setFilename(String f);
}
