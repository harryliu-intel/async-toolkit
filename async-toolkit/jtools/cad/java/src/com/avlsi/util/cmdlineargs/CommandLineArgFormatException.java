/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs;

/**
 * Exception thrown by {@link CommandLineArgsUtil} methods when a
 * required argument is in an invalid format for its type.
 *
 * @author Jesse Rosenstock
 **/
public final class CommandLineArgFormatException extends Exception {

    /**
     * The name of the argument that was in an invalid format.
     **/
    private final /*@ non_null @*/ String argName;

    /**
     * Class constructor.  
     **/
    public CommandLineArgFormatException(
            final /*@ non_null @*/ String argName,
            final /*@ non_null @*/ String detail,
            final /*@ non_null @*/ Throwable cause) {
        super(detail, cause);

        this.argName = argName;
    }

    /**
     * Class constructor.  
     **/
    public CommandLineArgFormatException(
            final /*@ non_null @*/ String argName,
            final /*@ non_null @*/ String detail) {
        super(detail);

        this.argName = argName;
    }

    /**
     * Class constructor.  
     **/
    public CommandLineArgFormatException(
            final /*@ non_null @*/ String argName,
            final /*@ non_null @*/ Throwable cause) {
        super(cause);

        this.argName = argName;
    }

    public /*@ non_null @*/ String getArgName() {
        return argName;
    }
}
