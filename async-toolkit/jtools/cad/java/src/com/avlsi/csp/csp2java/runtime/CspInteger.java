/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;
import com.avlsi.csp.csp2java.runtime.Fold.BinaryFunction; 
import com.avlsi.util.math.BigIntegerUtil;

/**
 * Class to represent an integer in csp.  This just contains a reference to
 * a BigInteger.
 *
 * <p>I think it would be better to store the csp variables as
 * <code>BigInteger[1]</code> and use BigInteger everywhere else, but
 * this will do for now. </p>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class CspInteger implements CspCloneableValue {
    /**
     * A constant version that will throw an <code>AssertionError</code> on
     * attempts to modify it.  To be efficient, <code>JavaEmitter</code> emits
     * code that use some common constants instead of constructing new
     * CspInteger objects.  However, if, for some reason, a CSP variable is
     * assigned one of these constants, and later the variable is modified,
     * then the value of these constants will also change, which is
     * undesirable.  This makes sure that does not happen without notice.
     **/
    private static class Constant extends CspInteger {
        public Constant(final BigInteger val) {
            super(val);
        }
        protected void setValue(final BigInteger val) {
            throw new AssertionError("Trying to modify CspInteger constant: " +
                                     getValue());
        }
    }

    public static final CspInteger ZERO = new Constant(BigInteger.ZERO);
    public static final CspInteger ONE = new Constant(BigInteger.ONE);
    public static final CspInteger FALSE = ZERO;
    public static final CspInteger TRUE = new Constant(new BigInteger("-1"));

    private BigInteger val;

    public CspInteger() {
        this(BigInteger.ZERO);
    }

    public CspInteger(final CspInteger v) {
        this(v.getValue());
    }

    public CspInteger(final BigInteger val) {
        this.val = val;
    }

    public CspInteger(final byte v) {
        this(byteConsts[v - (int) Byte.MIN_VALUE]);
    }

    public void setValue(final CspValue v) {
        setValue((CspInteger) v);
    }

    public void setValue(final CspInteger v) {
        setValue(v.getValue());
    }

    public void setValue(CspValue v, BinaryFunction modifier) {
        setValue((CspInteger) modifier.evaluate(this, v));
    }
    
    protected void setValue(final BigInteger val) {
        this.val = val;
    }

    protected BigInteger getValue() {
        return val;
    }

    public CspInteger add(final CspInteger v) {
        return new CspInteger(getValue().add(v.getValue()));
    }

    public CspInteger subtract(final CspInteger v) {
        return new CspInteger(getValue().subtract(v.getValue()));
    }

    public CspInteger multiply(final CspInteger v) {
        return new CspInteger(getValue().multiply(v.getValue()));
    }

    public CspInteger divide(final CspInteger v) {
        // x / 0 == 0
        final BigInteger vval = v.getValue();
        if (vval.equals(BigInteger.ZERO))
            return new CspInteger(BigInteger.ZERO);
        else
            return new CspInteger(getValue().divide(vval));
    }

    public CspInteger remainder(final CspInteger v) {
        // x % 0 == x
        final BigInteger myVal = getValue();
        final BigInteger yourVal = v.getValue();
        if (yourVal.equals(BigInteger.ZERO))
            return new CspInteger(myVal);
        else
            return new CspInteger(myVal.remainder(yourVal));
    }

    public CspInteger negate() {
        return new CspInteger(getValue().negate());
    }

    public CspInteger and(final CspInteger v) {
        return new CspInteger(getValue().and(v.getValue()));
    }

    public CspInteger or(final CspInteger v) {
        return new CspInteger(getValue().or(v.getValue()));
    }

    public CspInteger xor(final CspInteger v) {
        return new CspInteger(getValue().xor(v.getValue()));
    }

    public CspInteger not() {
        return new CspInteger(getValue().not());
    }

    public CspInteger shiftLeft(final int shiftAmount) {
        return new CspInteger(getValue().shiftLeft(shiftAmount));
    }

    public CspInteger shiftLeft(final CspInteger shiftAmount) {
        return shiftLeft(shiftAmount.intValue());
    }

    public CspInteger shiftRight(final int shiftAmount) {
        return new CspInteger(getValue().shiftRight(shiftAmount));
    }

    public CspInteger shiftRight(final CspInteger shiftAmount) {
        return shiftRight(shiftAmount.intValue());
    }

    public CspInteger pow(final int exponent) {
        if (exponent >= 0)
            return new CspInteger(getValue().pow(exponent));
        else if (getValue().equals(BigInteger.ZERO))
            return new CspInteger(BigInteger.ZERO);
        else
            return new CspInteger(BigInteger.ONE.divide(getValue().pow(-exponent)));
    }

    public CspInteger extractBits(final CspInteger min) {
        // x[n:m] --> ((x & ((1 << (n + 1)) - 1)) >> m) where m <= n
        return extractBits(min, min);
    }

    public CspInteger extractBits(final CspInteger min, final CspInteger max) {
        return extractBits(min.intValue(), max.intValue());
    }

    public CspInteger extractBits(final int min, final int max) {
        // x[n:m] --> ((x & ((1 << (n + 1)) - 1)) >> m) where m <= n
        return this.and(ONE.shiftLeft(max + 1).subtract(ONE))
                   .shiftRight(min);
    }

    public void assignBits(final CspInteger min, final CspInteger expr) {
        assignBits(min, min, expr);
    }

    public void assignBits(final CspInteger min, final CspInteger expr,
                           final BinaryFunction modifier) {
        assignBits(min, min, expr, modifier);
    }

    public void assignBits(final CspInteger min, final CspInteger max,
                           final CspInteger expr) {
        assignBits(min.intValue(), max.intValue(), expr);
    }

    public void assignBits(final int min, final int max,
                           final CspInteger expr) {
        // x[n:m] := expr 
        //      --> ((x & ~((1 << (n + 1)) - (1 << m))) 
        //           | ((expr << m) & ((1 << (n + 1)) - (1 << m)))) 
        // where m <= n
        assert (max < Integer.MAX_VALUE);
        final CspInteger mask = ONE.shiftLeft(max + 1)
                                   .subtract(ONE.shiftLeft(min));
        setValue(this.and(mask.not())
                     .or(expr.shiftLeft(min).and(mask)));
    }

    public void assignBits(final CspInteger min, final CspInteger max,
                           final CspInteger expr,
                           final BinaryFunction modifier) {
        // x[n:m] := expr 
        //      --> ((x & ~((1 << (n + 1)) - (1 << m))) 
        //           | ((expr << m) & ((1 << (n + 1)) - (1 << m)))) 
        // where m <= n
        final int minValue = min.intValue();
        final CspInteger mask = ONE.shiftLeft(max.add(ONE).intValue())
                                   .subtract(ONE.shiftLeft(minValue));
        final CspInteger curr = extractBits(min, max);
        final CspInteger eval = (CspInteger) modifier.evaluate(curr, expr);
        setValue(this.and(mask.not())
                     .or(eval.shiftLeft(minValue).and(mask)));
    }

    public CspInteger log2() {
        return CspInteger.valueOf(BigIntegerUtil.log2(getValue()));
    }

    public CspInteger log4() {
        return CspInteger.valueOf(BigIntegerUtil.log4(getValue()));
    }

    public int compareTo(final CspInteger v) {
        return getValue().compareTo(v.getValue());
    }

    public int intValue() {
        return getValue().intValue();
    }

    public long longValue() {
        return getValue().longValue();
    }

    public BigInteger toBigInteger() {
        return getValue();
    }

    public static CspInteger valueOf(final long val) {
        return new CspInteger(BigInteger.valueOf(val));
    }

    public String toString() {
        return getValue().toString();
    }

    public String toString(final int radix) {
        return getValue().toString(radix);
    }

    public boolean equals(final Object o) {
        return o instanceof CspInteger && equals((CspInteger) o);
    }

    public boolean equals(final CspInteger v) {
        return v != null && getValue().equals(v.getValue());
    }

    public int bitLength() {
        return getValue().bitLength();
    }

    public boolean booleanValue() {
        return !this.equals(FALSE);
    }

    public static CspInteger valueOf(final boolean val) {
        return val ? TRUE : FALSE;
    }

    public CspCloneableValue duplicate() {
        return new CspInteger(getValue());
    }

    private static final BigInteger[] byteConsts;

    static {
        byteConsts = new BigInteger[256];
        byte[] tmp = new byte[1];
        for (int i = Byte.MIN_VALUE; i <= Byte.MAX_VALUE; i++) {
            BigInteger bi;
            if (i == 0) {
                bi = BigInteger.ZERO;
            } else if (i == 1) {
                bi = BigInteger.ONE;
            } else {
                tmp[0] = (byte) i;
                bi = new BigInteger(tmp);
            }
            assert bi.intValue() == i;
            byteConsts[i - Byte.MIN_VALUE] = bi;
        }
    }
}
