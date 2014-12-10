/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import java.util.LinkedList;
import antlr.collections.AST;
import antlr.RecognitionException;
import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;

/**
 * Lets tools handle multiple versions of cast with specific code only
 * for constructors.
 **/
public interface CastTreeParserInterface {
    void setCastParserEnvironment(final CastParserEnvironment cpe);
    Environment  goal(AST _t, CellImpl cell, String moduleName,
                      LinkedList fileList)
        throws RecognitionException;
    void doInstantiation(AST t, String moduleName,
                         Environment env, CellImpl parent,
                         CellInterface envContainer)
        throws RecognitionException;
}
