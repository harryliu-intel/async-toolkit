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
public final class InvalidCommandLineArgException extends Exception {

    /**
     * The name of the argument that had an invalid value.
     **/
    private final /*@ non_null @*/ String argName;

    /**
     * The value of the argument that was invalid.
     **/
    private final /*@ non_null @*/ String argValue;

    /**
     * Class constructor.  
     **/
    public InvalidCommandLineArgException(
            final /*@ non_null @*/ String detail,
            final /*@ non_null @*/ String argName,
            final /*@ non_null @*/ String argValue) {
        super(detail);

        this.argName = argName;
        this.argValue = argValue;
    }

    /**
     * Class constructor.  
     **/
    public InvalidCommandLineArgException(
            final /*@ non_null @*/ String argName,
            final /*@ non_null @*/ String argValue) {
        this("Invalid value for argument " + argName, argName, argValue);
    }

    public /*@ non_null @*/ String getArgName() {
        return argName;
    }

    public /*@ non_null @*/ String getArgValue() {
        return argValue;
    }
}
