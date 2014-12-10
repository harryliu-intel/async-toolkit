/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


public class ArgumentMismatchException extends Exception {
    public ArgumentMismatchException(final String name,
                                     int expected,
                                     int found) {
        super("ShapeFunction " + name + " expects " + expected + " arguments, found " + found + " arguments.");
    }
}
