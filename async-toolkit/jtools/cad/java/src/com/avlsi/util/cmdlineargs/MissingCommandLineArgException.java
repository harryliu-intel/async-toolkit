/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs;

/**
 * Exception thrown by {@link CommandLineArgsUtil} methods when a
 * required argument is missing.
 *
 * @author Jesse Rosenstock
 **/
public final class MissingCommandLineArgException extends Exception {

    /**
     * The name of the argument that was missing.
     **/
    private final /*@ non_null @*/ String argName;

    /**
     * Class constructor.  
     **/
    public MissingCommandLineArgException(
            final /*@ non_null @*/ String detail,
            final /*@ non_null @*/ String argName) {
        super(detail);

        this.argName = argName;
    }

    /**
     * Class constructor.  
     **/
    public MissingCommandLineArgException(
            final /*@ non_null @*/ String argName) {
        this("Required argument " + argName + " not supplied.", argName);
    }

    public /*@ non_null @*/ String getArgName() {
        return argName;
    }
}
