/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.text;

/**
 * Class to represent strings as byte arrays to save precious memory.
 * 
 * @todo jmr XXX make this into a real class.  The overhead isn't that bad.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 * @review kiniry 18-23 July 2002
 *
 * @bug kiniry 23 July 2002 - Memory and performance issues with
 * ByteArrayString.  In particular, equals(), lastIndexOf(), and
 * indexOf() account for 10Ms of calls and 3% of the run-time of a
 * moderate PrsToNet run.
 * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1169">Bug#1169</a>
 **/
public final class ByteArrayString {
    /**
     * This class should not be instantiated.
     **/
    private ByteArrayString() { }

    /**
     * Return the specified subarray [beginIndex, endIndex).
     * @throws IndexOutOfBoundsException
     *         If <code>beginIndex &lt; 0</code>
     *         or <code>endIndex &gt; length()</code>
     *         or <code>beginIndex &gt; endIndex</code>.
     **/
    public static byte[] substring(final byte[] ba,
                                   final int beginIndex, 
                                   final int endIndex) {
        if (beginIndex < 0 || endIndex > ba.length)
            throw new IndexOutOfBoundsException();
        if (beginIndex > endIndex)
            throw new IndexOutOfBoundsException();


        final byte[] sub = new byte[endIndex - beginIndex];

        int j = beginIndex;
        for (int i = 0; i < sub.length; ++i, ++j)
            sub[i] = ba[j];

        return sub;
    }

    /**
     * Returns the first index of b in array ba.
     *
     * @param ba unknown.
     * @param b unknown.
     *
     * @bug kiniry 23 July 2002 - Memory and performance issues with
     * ByteArrayString.  In particular, equals(), lastIndexOf(), and indexOf()
     * account for 10Ms of calls and 3% of the run-time of a moderate PrsToNet
     * run.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1169">Bug#1169</a>
     **/
    public static int indexOf(final byte[] ba, final byte b) {
        for (int i = 0; i < ba.length; ++i)
            if (ba[i] == b)
                return i;

        return -1;
    }

    /**
     * Returns the last index of b in array ba.
     *
     * @param ba unknown.
     * @param b unknown.
     *
     * @bug kiniry 23 July 2002 - Memory and performance issues with
     * ByteArrayString.  In particular, equals(), lastIndexOf(), and indexOf()
     * account for 10Ms of calls and 3% of the run-time of a moderate PrsToNet
     * run.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1169">Bug#1169</a>
     **/
    public static int lastIndexOf(final byte[] ba, final byte b) {
        for (int i = ba.length - 1; i >= 0; --i)
            if (ba[i] == b)
                return i;

        return -1;
    }

    /**
     * Returns a string representation of the byte array
     *
     * @param ba unknown.
     * @return unknown.
     * @pre ba != null
     * @ensures Result and `ba' are equivalent in some way.
     **/
    public static String getString(final byte[] ba) {
        final StringBuffer sb = new StringBuffer(ba.length);

        for (int i = 0; i < ba.length; ++i)
            sb.append((char) ba[i]);

        return sb.toString();
    }

    /**
     * Turns the String `s' into a byte array.
     *
     * @param s the string to convert into a byte array.
     * @return the new byte array equivalent to `s'.
     * @pre s != null
     * @ensures Result and `s' are equivalent in some way.
     **/
    public static byte[] fromString(final String s)
        throws UnrepresentableCharException {
        final byte[] ba = new byte[s.length()];

        for (int i = 0; i < s.length(); ++i) {
            final char ch = s.charAt(i);

            if (ch > Byte.MAX_VALUE)
                throw new UnrepresentableCharException(ch);

            ba[i] = (byte) ch;
        }

        return ba;
    }

    /**
     * @todo Undocumented.
     *
     * @param ba1 unknown.
     * @param ba2 unknown.
     *
     * @bug kiniry 23 July 2002 - Memory and performance issues with
     * ByteArrayString.  In particular, equals(), lastIndexOf(), and indexOf()
     * account for 10Ms of calls and 3% of the run-time of a moderate PrsToNet
     * run.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1169">Bug#1169</a>
     */
    public static boolean equals(final byte[] ba1, final byte[] ba2) {
        if (ba1.length != ba2.length)
            return false;

        for (int i = 0; i < ba1.length; ++i)
            if (ba1[i] != ba2[i])
                return false;

        return true;
    }

    public static int compare(final byte[] ba1, final byte[] ba2) {
        final int len1 = ba1.length;
        final int len2 = ba2.length;
        final int n = Math.min(len1, len2);

        for (int i = 0; i < n; ++i) {
            final byte b1 = ba1[i];
            final byte b2 = ba2[i];

            if (b1 != b2)
                return b1 - b2;

        }

        return len1 - len2;
    }
}
