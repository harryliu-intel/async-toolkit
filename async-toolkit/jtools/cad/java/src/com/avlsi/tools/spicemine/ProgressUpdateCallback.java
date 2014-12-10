/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.spicemine;

import java.io.PrintStream;

/*****************************************************************
* Simple UI progress update callback class.
*****************************************************************/
class ProgressUpdateCallback {

    public static final char backspace = '';
    private int lastLength;
    private final PrintStream outStream;

    ProgressUpdateCallback(final PrintStream outStream) {
        this.outStream = outStream;
    }

    protected void printToStream(final String s) {
        clearLast();
        outStream.print("["+s+"]");
        lastLength = s.length()+2;
    }

    public void clearLast() {
        for (int i=0; i<lastLength; i++) outStream.print(backspace);
        for (int i=0; i<lastLength; i++) outStream.print(' ');
        for (int i=0; i<lastLength; i++) outStream.print(backspace);
        lastLength = 0;
    }
}
