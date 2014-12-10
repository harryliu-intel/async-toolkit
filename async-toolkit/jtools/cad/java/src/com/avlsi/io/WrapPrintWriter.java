/*
 * Copyright 2006 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.io;

import java.io.PrintWriter;

public class WrapPrintWriter implements Printable {
    private final PrintWriter w;

    public WrapPrintWriter(PrintWriter w) {
        this.w = w;
    }

    /** Flush the stream if it's not closed and check its error state. */
    public boolean     checkError() { return w.checkError(); }

    /** Close the stream. */
    public void        close() { w.close(); }

    /** Flush the stream. */
    public void        flush() { w.flush(); }

    /** Print a boolean value. */
    public void        print(boolean b) { w.print(b); }

    /** Print a character. */
    public void        print(char c) { w.print(c); }

    /** Print an array of characters. */
    public void        print(char[] s) { w.print(s); }

    /** Print a double-precision floating-point number. */
    public void        print(double d) { w.print(d); }

    /** Print a floating-point number. */
    public void        print(float f) { w.print(f); }

    /** Print an integer. */
    public void        print(int i) { w.print(i); }

    /** Print a long integer. */
    public void        print(long l) { w.print(l); }

    /** Print an object. */
    public void        print(Object obj) { w.print(obj); }

    /** Print a string. */
    public void        print(String s) { w.print(s); }

    /** Terminate the current line by writing the line separator string. */
    public void        println() { w.println(); }

    /** Print a boolean value and then terminate the line. */
    public void        println(boolean x) { w.println(x); }

    /** Print a character and then terminate the line. */
    public void        println(char x) { w.println(x); }

    /** Print an array of characters and then terminate the line. */
    public void        println(char[] x) { w.println(x); }

    /** Print a double-precision floating-point number and then terminate the line. */
    public void        println(double x) { w.println(x); }

    /** Print a floating-point number and then terminate the line. */
    public void        println(float x) { w.println(x); }

    /** Print an integer and then terminate the line. */
    public void        println(int x) { w.println(x); }

    /** Print a long integer and then terminate the line. */
    public void        println(long x) { w.println(x); }

    /** Print an Object and then terminate the line. */
    public void        println(Object x) { w.println(x); }

    /** Print a String and then terminate the line. */
    public void        println(String x) { w.println(x); }
}
