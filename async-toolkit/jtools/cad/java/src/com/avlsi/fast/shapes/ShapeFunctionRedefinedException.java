/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


public class ShapeFunctionRedefinedException extends Exception {
    public ShapeFunctionRedefinedException(final String func) {
        super("ShapeFunction " + func + " redefined.");
    }
}
