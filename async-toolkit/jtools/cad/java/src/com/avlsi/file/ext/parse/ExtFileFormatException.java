/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.ext.parse;

public class ExtFileFormatException extends Exception {
    private final String fileName;
    private final int lineNumber;
    private final String line;

    public ExtFileFormatException(final String detail,
                                  final String fileName,
                                  final int lineNumber,
                                  final String line,
                                  final Throwable cause)
    {
        super("File: " + fileName + "; line: " + lineNumber + ": " + detail,
              cause);
        this.fileName = fileName;
        this.lineNumber = lineNumber;
        this.line = line;
    }

    public ExtFileFormatException(final String detail,
                                  final String fileName,
                                  final int lineNumber,
                                  final String line)
    {
        this(detail, fileName, lineNumber, line, null);
    }

    public String getFileName() {
        return fileName;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public String getLine() {
        return line;
    }

    public String toString() {
        return getMessage() + "; line was: " + getLine();
    }
}
