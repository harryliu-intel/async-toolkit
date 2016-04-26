package com.avlsi.util.math;

import java.math.BigInteger;
import com.avlsi.util.container.CollectionUtils;

public class BigIntegerUtil {
    private BigIntegerUtil() { }

    public static int log2(BigInteger v) {
        return v.subtract(BigInteger.ONE).bitLength();
    }

    public static int log4(BigInteger v) {
        return (log2(v) + 1) / 2;
    }

    public static int log(BigInteger base, BigInteger v) {
        int result = 0;
        if (v.compareTo(BigInteger.ZERO) < 0) v = v.negate();
        else v = v.subtract(BigInteger.ONE);
        while (v.compareTo(BigInteger.ZERO) > 0) {
            v = v.divide(base);
            ++result;
        }
        return result;
    }

    public static boolean isPowerOf2(final BigInteger v) {
        return (v.signum() == -1 && v.bitCount() == v.bitLength()) ||
               (v.signum() == 1 && v.bitCount() == 1);
    }

    /**
     * Return <code>v</code> truncated to <code>width</code> bits.
     **/
    public static BigInteger truncate(BigInteger v, int width) {
        return v.mod(BigInteger.ONE.shiftLeft(width));
    }

    public static BigInteger truncate(BigInteger v, int width, boolean signed) {
        final BigInteger part = truncate(v, width);
        if (signed) {
            if (part.testBit(width - 1)) {
                return truncate(part.not().add(BigInteger.ONE), width).negate();
            } else {
                return part;
            }
        } else {
            return part;
        }
    }

    public static int safeIntValue(BigInteger v) {
        if (v.compareTo(BigInteger.valueOf(Integer.MIN_VALUE)) < 0 ||
            v.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
            throw new IllegalArgumentException(v + " too wide to fit into int");
        } else {
            return v.intValue();
        }
    }

    public static BigInteger fromASCII(final String s) {
        return new BigInteger(CollectionUtils.reverse(s.getBytes()));
    }
}
