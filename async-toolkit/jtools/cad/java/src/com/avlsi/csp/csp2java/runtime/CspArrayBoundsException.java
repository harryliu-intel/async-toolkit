package com.avlsi.csp.csp2java.runtime;

public class CspArrayBoundsException extends RuntimeException {
    final String filename;
    final int line, column;
    public CspArrayBoundsException(final CspInteger index,
                                   final int min,
                                   final int max,
                                   final String filename,
                                   final int line,
                                   final int column) {
        super("index " + index + " out of bounds " + min + ".." + max);
        this.filename = filename;
        this.line = line;
        this.column = column;
    }
}
