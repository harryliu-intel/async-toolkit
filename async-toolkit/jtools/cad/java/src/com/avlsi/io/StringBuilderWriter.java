package com.avlsi.io;

import java.io.IOException;
import java.io.Writer;

/**
 * Like StringWriter, except uses a StringBuilder instead of a StringBuffer to
 * hold the data written.  This class is also unsynchronized.
 **/
public final class StringBuilderWriter extends Writer {
    private final StringBuilder builder;

    public StringBuilderWriter() {
        builder = new StringBuilder();
    }

    public StringBuilderWriter(final int capacity) {
        builder = new StringBuilder(capacity);
    }

    public void write(int c) {
        builder.append((char) c);
    }

    public void write(char cbuf[]) {
        builder.append(cbuf);
    }

    public void write(char cbuf[], int off, int len) {
        builder.append(cbuf, off, len);
    }

    public void write(String str) {
        if (str == null) throw new NullPointerException();
        else builder.append(str);
    }

    public void write(String str, int off, int len) {
        builder.append(str, off, off + len);
    }

    public StringBuilderWriter append(CharSequence csq) {
        builder.append(csq);
        return this;
    }

    public StringBuilderWriter append(CharSequence csq, int start, int end) {
        builder.append(csq, start, end);
        return this; 
    }

    public StringBuilderWriter append(char c) {
        builder.append(c);
        return this;
    }

    public void flush() { }

    public void close() { }

    public StringBuilder getBuffer() {
        return builder;
    }

    public String toString() {
        return builder.toString();
    }
}
