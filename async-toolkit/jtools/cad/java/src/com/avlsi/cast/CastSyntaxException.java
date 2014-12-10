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

package com.avlsi.cast;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * Exception thrown by {@link CastFileParser} for syntax errors in cast
 * files.  Wraps another exception for the stack dump, but stores
 * its own file/line/column info so it can wrap exceptions without
 * file/line/column info, like CircularImportExceptions.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CastSyntaxException extends Exception {

    private final String fileName;
    private final int lineNumber;
    private final int columnNumber;

    public CastSyntaxException(final Exception e,
                               final String fileName,
                               final int lineNumber,
                               final int columnNumber) {
        super(e);
        this.fileName = fileName;
        this.lineNumber = lineNumber;
        this.columnNumber = columnNumber;
    }

    public String getFileName() {
        return fileName;
    }

    public int getColumnNumber() {
        return columnNumber;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public String toString() {
        return "CastSyntaxException["
            + getFileName() + ":"
            + getLineNumber() + ","
            + getColumnNumber() + "]: "
            + "wrapping " + getCause().getMessage();
    }

    public String getMessage() {
        return getCause().getMessage();
    }
}
