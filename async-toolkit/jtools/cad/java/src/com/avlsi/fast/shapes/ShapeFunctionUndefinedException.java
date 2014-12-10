/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


public class ShapeFunctionUndefinedException extends Exception {
    public ShapeFunctionUndefinedException(final String func) {
        super("ShapeFunction " + func + " undefined.");
    }
}
