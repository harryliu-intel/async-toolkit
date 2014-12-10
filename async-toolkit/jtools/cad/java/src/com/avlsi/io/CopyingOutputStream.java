/*
 * Copyright 2005 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.io;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Copy one OutputStream to multiple OutputStreams.
 */
public class CopyingOutputStream extends OutputStream {
    private final OutputStream[] streams;

    /**
     * Specifies the output streams you want to copy to.
     * This can be any number of streams, including 1 (which is
     * kind of pointless) or 0 (which gives you the effect of
     * /dev/null).
     */
    public CopyingOutputStream(OutputStream[] streams) {
        this.streams = streams;
    }

    public void close() throws IOException {
        for (int i = 0; i < streams.length; i++)
            streams[i].close();
    }

    public void flush() throws IOException {
        for (int i = 0; i < streams.length; i++)
            streams[i].flush();
    }

    public void write(byte[] b) throws IOException {
        for (int i = 0; i < streams.length; i++)
            streams[i].write(b);
    }

    public void write(byte[] b, int off, int len) throws IOException {
        for (int i = 0; i < streams.length; i++)
            streams[i].write(b, off, len);
    }

    public void write(int b) throws IOException {
        for (int i = 0; i < streams.length; i++)
            streams[i].write(b);
    }
}
