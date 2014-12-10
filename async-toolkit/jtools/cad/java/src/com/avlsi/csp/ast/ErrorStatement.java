/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * Error statement.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ErrorStatement 
    extends AbstractASTNode
    implements StatementInterface {

    /** File in which the error occurred. **/
    private final String filename;
    /** Line number on which the error occurred. **/
    private final int line;
    /** Column number at which the error occurred. **/
    private final int column;

    /**
     * Class constructor.
     *
     * @param filename file in which the error occurred
     * @param line  line number on which the error occurred
     * @param column  column number at which the error occurred
     **/
    public ErrorStatement(final String filename,
            final int line,
            final int column) {
        this.filename = filename;
        this.line = line;
        this.column = column;
    }

    /**
     * Returns file in which the error occurred.
     *
     * @return file in which the error occurred
     **/
    public String getFilename() {
        return filename;
    }

    /**
     * Returns line number on which the error occurred.
     *
     * @return line number on which the error occurred
     **/
    public int getLine() {
        return line;
    }

    /**
     * Returns column number at which the error occurred.
     *
     * @return column number at which the error occurred
     **/
    public int getColumn() {
        return column;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitErrorStatement(this);
    }
}
