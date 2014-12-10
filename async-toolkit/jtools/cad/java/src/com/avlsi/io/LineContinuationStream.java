/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.io;

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.PushbackInputStream;

public class LineContinuationStream extends FilterInputStream {
    private int continueChar;
    private InputStream real;
    private boolean atEOF;

    public LineContinuationStream(InputStream real, int continueChar) {
        super(new PushbackInputStream(real));
        this.real = real;
        this.continueChar = continueChar;
        atEOF = false;
    }

    /*
    public int available() throws IOException {
        int avail = real.available();
        return (in.available() - avail) + avail / 2;
    }
    */

    public int read() throws IOException {
        if (atEOF) return -1;

        int a = 0, b = 0;
        if ((a = in.read()) == '\n') {
            if ((b = in.read()) == continueChar) {
                a = ' ';
            } else if (b < 0) {
                atEOF = true;
            } else {
                ((PushbackInputStream) in).unread(b);
            }
        }
        return a;
    }

    public long skip(long n) throws IOException {
        long count = 0;
        while (n > 0) {
            if (read() == -1) break;
            n--;
            count++;
        }
        return count;
    }

    public int read(byte[] b, int off, int len) throws IOException {
        int count = 0, c = 0;
        while (len > 0) {
            if ((c = read()) < 0) break;
            b[off] = (byte) c;
            off++;
            len--;
            count++;
        }
        if (count == 0 && c < 0) return -1;
        else return count;
    }
}
