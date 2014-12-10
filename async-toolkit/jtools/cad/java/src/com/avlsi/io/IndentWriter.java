/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.io;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

/**
 * Class that provides a general way to indent output.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public final class IndentWriter extends FilterWriter {
    /** Current level of indenting. */
    private int level;

    /** The "tab" string. */
    private final String tab;

    /** Is the last character written a newline? */
    private boolean nl;

    /**
     * Constructs a new IndentWriter object.  The indent string is set to 2
     * spaces.
     * @param out a Writer object to provide the underlying stream.
     **/
    public IndentWriter(Writer out) {
        this(out, "  ");
    }

    /**
     * Constructs a new IndentWriter object.
     * @param out a Writer object to provide the underlying stream.
     * @param tab a String to print for each indent level.
     **/
    public IndentWriter(Writer out, String tab) {
        super(out);
        this.level = 0;
        this.tab = tab;
        this.nl = false;
    }

    public void write(char[] cbuf, int off, int len) throws IOException {
        for (int i = 0; i < len; ++i, ++off) write(cbuf[off]);
    }

    public void write(String str, int off, int len) throws IOException {
        for (int i = 0; i < len; ++i, ++off) write(str.charAt(off));
    }

    public void write(int c) throws IOException {
        if (c == '\n') {
            nl = true;
        } else {
            if (nl) {
                for (int i = 0; i < level; ++i)
                    out.write(tab, 0, tab.length());
                nl = false;
            }
        }
        out.write(c);
    }

    /**
     * Sets the current indenting level.  Clamp the level to 0.
     * @param level New level of indenting.
     **/
    public void setLevel(int level) {
        if (level < 0) level = 0;
        this.level = level;
    }

    /**
     * Returns the current indenting level.
     * @return Current level of indenting.
     **/
    public int getLevel() {
        return level;
    }

    /**
     * Increment the indenting level.
     **/
    public void nextLevel() {
        setLevel(getLevel() + 1);
    }

    /**
     * Decrement the indenting level.
     **/
    public void prevLevel() {
        setLevel(getLevel() - 1);
    }
}
