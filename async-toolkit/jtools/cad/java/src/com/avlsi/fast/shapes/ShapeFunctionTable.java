// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


import com.avlsi.fast.shapes.ShapeFunction;
import com.avlsi.fast.shapes.ShapeFunctionUndefinedException;

/**
   Interface to a symbol table of ShapeFunctions.
 */
public interface ShapeFunctionTable {
    /**
       Finds the ShapeFunction associated with a name.
       @param name Name of the function to lookup.
       @return The ShapeFunction associated with the specified name.
       @throws ShapeFunctionUndefinedException if the name was not found.
     */
    ShapeFunction lookup(final String name)
        throws ShapeFunctionUndefinedException;
}
