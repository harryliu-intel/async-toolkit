/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;


import java.util.HashMap;
import java.util.Map;

import com.avlsi.fast.shapes.ShapeFunction;
import com.avlsi.fast.shapes.WriteableShapeFunctionTable;
import com.avlsi.fast.shapes.ShapeFunctionRedefinedException;
import com.avlsi.fast.shapes.ShapeFunctionUndefinedException;

/**
   Default implementation of WriteableShapeFunctionTable.  The mapping is
   stored in a HashMap.
 */
public class WriteableShapeFunctionTableImpl
implements WriteableShapeFunctionTable {
    protected Map storage;

    public WriteableShapeFunctionTableImpl() {
        storage = new HashMap();
    }

    public ShapeFunction lookup(final String name)
        throws ShapeFunctionUndefinedException {
        ShapeFunction func = (ShapeFunction) storage.get(name);
        if (func == null) {
            throw new ShapeFunctionUndefinedException(name);
        }
        return func;
    }

    public void define(final String name, final ShapeFunction func) 
        throws ShapeFunctionRedefinedException {
        if (storage.containsKey(name)) {
            throw new ShapeFunctionRedefinedException(name);
        }
        storage.put(name, func);
    }
}
