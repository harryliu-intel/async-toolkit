/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.io;

/**
 * It's always galled me that PrintStream and PrintWriter don't share
 * a common interface.  The "correct" thing to use is usually a PrintWriter,
 * but since System.err and System.out are PrintStreams, it's important
 * to be able to support both.  So, I'd like to be able to just call
 * println() on something without caring which it is.
 *
 * So, this interface just contains all the methods that PrintStream and
 * PrintWriter have in common.
 *
 * See WrapPrintStream and WrapPrintWriter.
 */

public interface Printable {
    /** Flush the stream if it's not closed and check its error state. */
    boolean 	checkError();

    /** Close the stream. */
    void 	close();

    /** Flush the stream. */
    void 	flush();

    /** Print a boolean value. */
    void 	print(boolean b);

    /** Print a character. */
    void 	print(char c);

    /** Print an array of characters. */
    void 	print(char[] s);

    /** Print a double-precision floating-point number. */
    void 	print(double d);

    /** Print a floating-point number. */
    void 	print(float f);

    /** Print an integer. */
    void 	print(int i);

    /** Print a long integer. */
    void 	print(long l);

    /** Print an object. */
    void 	print(Object obj);

    /** Print a string. */
    void 	print(String s);

    /** Terminate the current line by writing the line separator string. */
    void 	println();

    /** Print a boolean value and then terminate the line. */
    void 	println(boolean x);

    /** Print a character and then terminate the line. */
    void 	println(char x);

    /** Print an array of characters and then terminate the line. */
    void 	println(char[] x);

    /** Print a double-precision floating-point number and then terminate the line. */
    void 	println(double x);

    /** Print a floating-point number and then terminate the line. */
    void 	println(float x);

    /** Print an integer and then terminate the line. */
    void 	println(int x);

    /** Print a long integer and then terminate the line. */
    void 	println(long x);

    /** Print an Object and then terminate the line. */
    void 	println(Object x);

    /** Print a String and then terminate the line. */
    void 	println(String x);
}
