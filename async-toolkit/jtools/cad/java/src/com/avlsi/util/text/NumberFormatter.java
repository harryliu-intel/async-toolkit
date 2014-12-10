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

import java.math.BigInteger;
import java.text.DecimalFormat;

/**
 * Class to format numbers in a reasonable way.  Provides printf-like
 * formatting.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class NumberFormatter {
    /**
     * This class should not be instantiated.
     **/
    private NumberFormatter() { }

    /**
     * Formats a double in a "pretty" way, not unlike that of C's
     * printf %g.  The conversion is that described by 
     * {@link java.lang.Double#toString} except for the number of digits
     * used to represent the magnitude.  At most <code>maxPrecision</code>
     * digits appear after the decimal point.
     *
     * @param   d   the <code>double</code> to be converted.
     * @param   maxPrecision   the maximum number of digits to appear 
     *          after the decimal point.
     * @return  a string representation of the argument.
     **/
    public static String format(final double d, final int maxPrecision) {
        // style e: [-]d.ddddddEd, one d before, maxPrecision after
        // style f: [-]ddd.dddddd

        // use style e if |d| < 1e-3 or >= 1e7, otherwise use style f

        // deal with NaN, +/- inf, +/- 0
        if (Double.isNaN(d))
            return "NaN";
        else if (Double.isInfinite(d))
            return d > 0.0 ? "Infinity" : "-Infinity";
        else if (d == 0.0)
            return 1.0 / d > 0.0 ? "0.0" : "-0.0";
        else {
            final String pattern;
            if (Math.abs(d) < 1e-3 || Math.abs(d) >= 1e7) {
                // style e
                pattern = "0." + StringUtil.repeatString("#", maxPrecision)
                    + "E0";
            } else {
                // style f
                pattern = "0." + StringUtil.repeatString("#", maxPrecision);
                // @review jmr XXX should this last part have 1 or more digits?
            }

            return new DecimalFormat(pattern).format(d);
        }
    }

    /**
     * Formats a double in a "pretty" way, not unlike that of C's
     * printf %g.  The conversion is that described by 
     * {@link java.lang.Double#toString} except for the number of digits
     * used to represent the magnitude.  At most 6 digits appear
     * after the decimal point.
     *
     * @param   d   the <code>double</code> to be converted.
     * @return  a string representation of the argument.
     **/
    public static String format(final double d) {
        return format(d, 6);
    }

    /**
     * Private method to pad hex strings.
     **/
    private static String padHexString(String s, final int length) {
        if (s.length() < length)
            s = StringUtil.repeatString("0", length - s.length()) + s;
	return s.toUpperCase();
    }

    /**
     * Returns a zero padded, uppercase, hex String for a BigInteger.
     **/
    public static String toHexString(final BigInteger n, final int length) {
	return padHexString(n.toString(16),length);
    }

    /**
     * Returns a zero padded, uppercase, hex String for an integer.
     **/
    public static String toHexString(final int n, final int length) {
	return padHexString(Integer.toHexString(n),length);
    }

    /**
     * Returns a zero padded, uppercase, hex String for a long.
     **/
    public static String toHexString(final long n, final int length) {
	return padHexString(Long.toHexString(n),length);
    }

    /**
     * Returns a lowercase hex String form a byte[].  The array is in big
     * endian, i.e., b[0] contains the most significant byte.  Each byte is
     * converted to 2 digits.
     **/
    public static String toHexString(final byte[] b) {
        final StringBuffer sb = new StringBuffer(b.length * 2);
        for (int i = 0; i < b.length; ++i) {
            sb.append(Character.forDigit((b[i] & 0xf0) >>> 4, 16));
            sb.append(Character.forDigit((b[i] & 0x0f), 16));
        }
        return sb.toString();
    }

    /**
     * Returns a zero padded octal String for a BigInteger.
     **/
    public static String toOctalString(final BigInteger n, final int length) {
	return padHexString(n.toString(8),length);
    }

    /**
     * Returns a zero padded octal String for an integer.
     **/
    public static String toOctalString(final int n, final int length) {
	return padHexString(Integer.toOctalString(n),length);
    }

    /**
     * Returns a zero padded octal String for a long.
     **/
    public static String toOctalString(final long n, final int length) {
	return padHexString(Long.toOctalString(n),length);
    }

    /**
     * Returns a zero padded binary String for a BigInteger.
     **/
    public static String toBinaryString(final BigInteger n, final int length) {
	return padHexString(n.toString(2),length);
    }

    /**
     * Returns a zero padded binary String for an integer.
     **/
    public static String toBinaryString(final int n, final int length) {
	return padHexString(Integer.toBinaryString(n),length);
    }

    /**
     * Returns a zero padded binary String for a long.
     **/
    public static String toBinaryString(final long n, final int length) {
	return padHexString(Long.toBinaryString(n),length);
    }
}
