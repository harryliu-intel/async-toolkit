/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.debug;

/**
 * Provides debugging functionality, like assertions.  
 *
 * @author Jesse Rosenstock
 * @author Aaron Denney
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Name:  $ $Date$
 **/

public final class Debug {

    // No attributes

    // No public constructors---only a static class.

    /**
     * DO NOT USE, this is a static class.
     * @deprecated Do not use, this is a static class.
     **/
    private Debug() {
        throw new AssertionError("Debug self-instantiation");
    }

    // Public Methods

    public static final void log(String s) {
        System.err.println(s);
    }

    /**
     * Indicate that the given method is deprecated and should
     * <strong>never</strong> be called.
     **/

    public static final void deprecated() {
        fail();
    }

    /**
     * Fail unconditionally.
     **/

    public static final void fail() {
        // don't use assert false so assertion status won't matter
        throw new AssertionError();
    }

    /**
     * Fail unconditionally with a message.
     *
     * @param message the message to be logged.
     **/

    public static final void fail(String message) {
        // don't use assert false : message so assertion status won't matter
        throw new AssertionError((Object) message);
    }

    /**
     * Indicate that a given method is unimplemented.  Should be called be all
     * methods that have not yet been implemented within a system.
     *
     * @post false
     **/
    public static final void unimplemented() {
        System.err.println("The following method was called " +
                           "and it is not yet implemented:");
        StackTraceElement [] stackTrace = 
            (new NoSuchMethodException()).getStackTrace();
        if (stackTrace.length > 0)
            System.err.println(stackTrace[0].toString());
        else
            System.err.println("Stack trace unavailable.");
        // don't use assert false so assertion status won't matter
        throw new AssertionError();
    }

    public static final void assertTrue(final boolean b) {
        assert b;
    }

    public static final void assertTrue(final boolean b, final String message) {
        assert b : message;
    }

    public static final void assertNotNull(final Object o, final String message) {
        assert o != null : message;
    }

    public static final void assertNotNull(final Object o) {
        assert o != null;
    }

    /**
     * Prints message when mask and value both have a bit set;
     * Used to fine grain control of messages.
     *
     * Usually used for debugging messages.  Each bit in the integer
     * is controls some subset of messages.
     *
     * @param mask    bits to check against.
     * @param current currently enabled bits.
     * @param message string to be printed.
     **/
    public static void maskPrint(final int mask, final int current,
                                 final String message) {
        if ((mask & current) != 0) {
            System.err.println(message);
        }
    }
}
