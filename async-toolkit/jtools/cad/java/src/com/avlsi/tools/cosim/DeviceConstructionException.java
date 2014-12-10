/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.cosim;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * Exception to be thrown if JavaCoSimDevice has trouble constructing
 * and starting the AbstractDevice it represents.  Either carries a
 * message in its own right or encapsulates another throwable in the
 * way SemanticWrapperException and others do.
 **/

public class DeviceConstructionException extends Exception {
    private final String className;
    private final String cellName;
    
    public DeviceConstructionException(final String message,
                                       final Class c) {
        this (message, c.getName(), "unknown cell");
    }

    public DeviceConstructionException(final String message,
                                       final String className,
                                       final String cellName) {
        this(message, null, className, cellName);
    }

    public DeviceConstructionException(final String message,
                                       final Throwable e,
                                       final String className,
                                       final String cellName) {
        super(message, e);
        this.className = className;
        this.cellName = cellName;
    }

    public String getMessage() {
        return toString();
    }

    public String toString() {
        if (getCause() == null)
            return getHeader();
        else
            return getHeader() + " ( wrapping:\n" +
                getCause().getMessage() + ")";
    }

    private String getHeader() {
        return getClass().getName() + ": " + super.getMessage();
    }
}
