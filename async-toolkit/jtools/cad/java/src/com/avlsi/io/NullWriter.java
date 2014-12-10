/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.io;

import java.io.Writer;

/**
   A writer that doesn't actually write.  Sort of like /dev/null.
 */
public final class NullWriter extends Writer {

    private static NullWriter singleton = null;

    private NullWriter() {
    }

    public void close() {
    }

    public void flush() {
    }

    public void write(char[] cbuf, int off, int len){
    } 

    public static NullWriter getInstance() {
        if (singleton == null) singleton = new NullWriter();
        return singleton;
    }
}
