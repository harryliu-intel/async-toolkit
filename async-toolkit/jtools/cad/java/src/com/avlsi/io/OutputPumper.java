/*
 * Copyright 2003-2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.io;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * A thread which copies an InputStream to an OutputStream
 */
public class OutputPumper extends Thread {
    private final OutputStream os;
    private final InputStream is;

    public OutputPumper(InputStream is, OutputStream os) {
        this.is = is;
        this.os = os;
    }

    public void run() {
        try {
            int c;
            while ((c = is.read()) >= 0) {
                os.write(c);
                os.flush();
            }
        } catch (Exception e) {
        }
    }
}
