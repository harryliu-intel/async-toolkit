/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


import com.avlsi.fast.shapes.ShapeFunction;
import com.avlsi.fast.shapes.ShapeFunctionRedefinedException;

/**
   Interface to a symbol table of ShapeFunctions.
 */
public interface WriteableShapeFunctionTable extends ShapeFunctionTable {
    /**
       Associate a ShapeFunction with a name.
       @param name Name to bind to.
       @param func ShapeFunction to be bound.
       @throws ShapeFunctionRedefinedException if the name was already bound.
     */
    void define(final String name, final ShapeFunction func)
        throws ShapeFunctionRedefinedException;
}
