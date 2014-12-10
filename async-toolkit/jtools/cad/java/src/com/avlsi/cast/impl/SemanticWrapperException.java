/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast.impl;

import java.io.PrintStream;
import java.io.PrintWriter;

import antlr.SemanticException;

import com.avlsi.util.exception.ExceptionUtils;

/**
 * Semantic exception class that wraps another exception.  This 
 * is used in cast_tree.g, and thrown for any errors in walking the tree.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class SemanticWrapperException extends SemanticException {

    /**
     * Construct a SemanticWrapperException wrapping the given
     * exception, with the error occuring at the specified 
     * filename, line, column.
     **/
    public SemanticWrapperException(final Exception e,
                                    final String filename,
                                    final int line,
                                    final int column)
    {
        this("", e, filename, line, column);
    }

    /**
     * Construct a SemanticWrapperException without knowing
     * file/line/column info.
     **/
    public SemanticWrapperException(final String message, final Exception e)
    {
        this(message, e, "<file unknown>", -1, -1);
    }

    /**
     * Class constructor
     **/
    public SemanticWrapperException(final String message,
                                    final Exception e,
                                    final String filename,
                                    final int line,
                                    final int column)
    {
        super(message, filename, line, column);
        initCause(e);
    }

    public String getMessage() {
        return toString();
    }

    /**
     * Returns the message that was passed into the constructor.
     * Needed to fix bug 3746.
     */
    public String getMessageOnly() {
        return super.getMessage();
    }

    public String toString() {
        return getHeader() + " ( wrapping:\n" + getCause() + ")";
    }

    private String getHeader() {
        return getClass().getName() + ": " + super.getMessage()
            + " at " + getPositionInfo();
    }

    private String getPositionInfo() {
        return getFilename() + ":" + getLine() + ":" + getColumn();
    }
}
