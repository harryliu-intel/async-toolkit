package com.avlsi.csp.util;

import java.lang.Math;
import java.math.BigInteger;

/**
 * Class implementing interval analysis on simple arithmetic and bitwise
 * operations with CSP integer semantics.  Sadly, not all operations will
 * return the tightest interval.
 **/
public class Interval {
    /**
     * Largest <code>int</code> as a <code>BigInteger</code>.  Useful for
     * comparison purposes before calling <code>intValue()</code>.
     **/
    private static final BigInteger MAX_INT =
        BigInteger.valueOf(Integer.MAX_VALUE);

    /**
     * Smallest <code>int</code> as a <code>BigInteger</code>.  Useful for
     * comparison purposes before calling <code>intValue()</code>.
     **/
    private static final BigInteger MIN_INT =
        BigInteger.valueOf(Integer.MIN_VALUE);

    /**
     * Interval [0..0].
     **/
    public static final Interval ZERO = new Interval(BigInteger.ZERO);

    /**
     * Interval [1..1].
     **/
    public static final Interval ONE = new Interval(BigInteger.ONE);

    /**
     * Returned if a computation is out of range.  For example, if the shift
     * amount or the exponent is greater than 32-bits.
     **/
    public static final Interval EXCEPTION = new Interval();

    /**
     * Left bound, inclusive.
     **/
    private final BigInteger from;

    /**
     * Right bound, inclusive.
     **/
    private final BigInteger to;

    /**
     * Private constructor only used to create <code>EXCEPTION</code>.  Use
     * <code>null</code> for the bounds to detect if <code>EXCEPTION</code> is
     * ever used incorrectly in a computation.
     **/
    private Interval() {
        this.from = null;
        this.to = null;
    }

    private static BigInteger bitsInt(final int bits, final boolean neg) {
        final BigInteger pow2 = BigInteger.ONE.shiftLeft(bits);
        return neg ? pow2.negate() : pow2.subtract(BigInteger.ONE);
    }

    /**
     * Construct an interval [0..2^bits-1].
     *
     * @param bits number of bits in the interval
     **/
    public Interval(final int bits) {
        this(false, bits);
    }

    /**
     * Construct an interval for a possibly signed integer expressed in 2's
     * complement that is <code>bits</code> wide (including the sign bit, if
     * any).  If signed, the interval is [-2^(bits-1)..2^(bits-1)-1]; if
     * unsigned, the interval is [0..2^bits-1].
     *
     * @param signed whether this is a signed integer
     * @param bits number of bits in the integer
     **/
    public Interval(final boolean signed, final int bits) {
        this(signed ? bitsInt(bits - 1, true) : BigInteger.ZERO,
             signed ? bitsInt(bits - 1, false) : bitsInt(bits, false));
    }

    /**
     * Construct an interval [from..to].
     *
     * @param from left bound of the interval, inclusive
     * @param to right bound of the interval, inclusive
     **/
    public Interval(final BigInteger from, final BigInteger to) {
        if (from.compareTo(to) < 0) {
            this.from = from;
            this.to = to;
        } else {
            this.from = to;
            this.to = from;
        }
    }

    /**
     * Construct an interval [val..val].
     *
     * @param val the single integer contained in the interval
     **/
    public Interval(final BigInteger val) {
        this(val, val);
    }

    /**
     * Returns the left bound.
     *
     * @return left bound of the interval
     **/
    public BigInteger getLeftBound() {
        return from;
    }

    /**
     * Returns the right bound.
     *
     * @return right bound of the interval
     **/
    public BigInteger getRightBound() {
        return to;
    }

    // Returns the minimum BigInteger
    private BigInteger min(final BigInteger... vals) {
        BigInteger result = null;
        for (BigInteger val : vals) {
            result = result == null ? val : result.min(val);
        }
        return result;
    }

    // Returns the maximum BigInteger
    private BigInteger max(final BigInteger... vals) {
        BigInteger result = null;
        for (BigInteger val : vals) {
            result = result == null ? val : result.max(val);
        }
        return result;
    }

    // Returns the union of intervals; a null interval is skipped
    private Interval union(final Interval... vals) {
        Interval result = null;
        for (Interval val : vals) {
            if (val != null) {
                if (result == null) result = val;
                else result = result.union(val);
            }
        }
        return result;
    }

    // Returns if any of the intervals are EXCEPTION
    private boolean checkException(final Interval... vals) {
        for (Interval v : vals) {
            if (v == EXCEPTION) return true;
        }
        return false;
    }

    /**
     * Returns the union of this interval and interval <param>r</param>.
     *
     * @param r interval to be unioned with this interval
     * @return the union interval or <code>EXCEPTION</code> if either interval
     * is <code>EXCEPTION</code>
     **/
    public Interval union(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        return new Interval(min(from, r.from), max(to, r.to));
    }

    public boolean equals(final Interval other) {
        return from.equals(other.from) && to.equals(other.to);
    }

    /**
     * Returns the interval containing the sum of an integer in this interval
     * addded to an integer in interval <param>r</param>.
     *
     * @param r interval containing integer to be added
     * @return the interval containing the sum or <code>EXCEPTION</code> if
     * either addend interval is <code>EXCEPTION</code>
     **/
    public Interval add(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        return new Interval(from.add(r.from), to.add(r.to));
    }

    // Partition this interval into 3 intervals: interval less than 0, interval
    // of 0, and interval greater than 0.
    private Interval[] partitionBySign() {
        final Interval[] result = new Interval[3];
        if (from.signum() == to.signum()) {
            result[from.signum() + 1] = this;
        } else {
            result[1] = Interval.ZERO;
            if (from.signum() == -1) {
                result[0] = new Interval(from, BigInteger.valueOf(-1));
            }
            if (to.signum() == 1) {
                result[2] = new Interval(BigInteger.ONE, to);
            }
        }
        return result;
    }

    // Perform an interval divide; the divisor interval must not contain 0
    private Interval simpleDivide(final Interval r) {
        final BigInteger q1 = from.divide(r.from);
        final BigInteger q2 = from.divide(r.to);
        final BigInteger q3 = to.divide(r.from);
        final BigInteger q4 = to.divide(r.to);
        return new Interval(min(q1, q2, q3, q4), max(q1, q2, q3, q4));
    }

    /**
     * Returns the interval containing the quotient of an integer in this
     * interval divided by an integer in interval <param>r</param>.  Note that
     * division by 0 is defined to be 0.
     *
     * @param r interval containing the divisor
     * @return the interval containing the quotient or <code>EXCEPTION</code>
     * if either the dividend or the divisor interval is <code>EXCEPTION</code>
     **/
    public Interval divide(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        final Interval[] divisor = r.partitionBySign();
        final Interval[] q = new Interval[3];
        if (divisor[0] != null) {
            q[0] = simpleDivide(divisor[0]);
        }
        if (divisor[1] != null) {
            q[1] = Interval.ZERO;
        }
        if (divisor[2] != null) {
            q[2] = simpleDivide(divisor[2]);
        }

        return union(q);
    }

    /**
     * Returns the interval containing the product of an integer in this
     * interval multiplied by an integer in interval <param>r</param>.
     *
     * @param r interval containing the term to be multiplied
     * @return the interval containing the product or <code>EXCEPTION</code>
     * if either term interval is <code>EXCEPTION</code>
     **/
    public Interval multiply(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        final BigInteger p1 = from.multiply(r.from);
        final BigInteger p2 = from.multiply(r.to);
        final BigInteger p3 = to.multiply(r.from);
        final BigInteger p4 = to.multiply(r.to);
        return new Interval(min(p1, p2, p3, p4), max(p1, p2, p3, p4));
    }

    /**
     * Returns the interval containing the remainder of an integer in this
     * interval divided by an integer in interval <param>r</param>.  Note that
     * the remainder equals the dividend if the divisor is 0.
     *
     * @param r interval containing the divisor
     * @return the interval containing the remainder or <code>EXCEPTION</code>
     * if either the dividend or the divisor interval is <code>EXCEPTION</code>
     **/
    public Interval remainder(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        final Interval[] dividend = partitionBySign();
        final Interval[] divisor = r.partitionBySign();
        final Interval[] rem = new Interval[6];
        if (dividend[0] != null) {
            if (divisor[0] != null) rem[0] = divisor[0].add(ONE);
            if (divisor[2] != null) rem[1] = divisor[2].negate().add(ONE);
        }
        if (dividend[2] != null) {
            if (divisor[0] != null) rem[2] = divisor[0].negate().subtract(ONE);
            if (divisor[2] != null) rem[3] = divisor[2].subtract(ONE);
        }
        if (dividend[1] != null) rem[4] = ZERO;
        if (divisor[1] != null) rem[5] = this;
        return union(rem);
    }

    /**
     * Returns the interval containing the result of an integer in this
     * interval shifted left by an integer in interval <param>r</param>.  A
     * negative left shift is defined as a right shift.
     *
     * @param r interval containing the shift amount
     * @return the interval containing the result of the left shift or
     * <code>EXCEPTION</code> if either interval is <code>EXCEPTION</code> or
     * if the shift amount could be less than -2^31 or greater than 2^31-1.
     **/
    public Interval shiftLeft(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        if (r.from.compareTo(MIN_INT) < 0 || r.to.compareTo(MAX_INT) > 0) {
            return EXCEPTION;
        }

        final BigInteger s1 = from.shiftLeft(r.from.intValue());
        final BigInteger s2 = from.shiftLeft(r.to.intValue());
        final BigInteger s3 = to.shiftLeft(r.from.intValue());
        final BigInteger s4 = to.shiftLeft(r.to.intValue());
        return new Interval(min(s1, s2, s3, s4), max(s1, s2, s3, s4));
    }

    /**
     * Returns the interval containing the result of an integer in this
     * interval shifted right by an integer in interval <param>r</param>.  A
     * negative right shift is defined as a left shift.
     *
     * @param r interval containing the shift amount
     * @return the interval containing the result of the right shift or
     * <code>EXCEPTION</code> if either interval is <code>EXCEPTION</code> or
     * if the shift amount could be less than -2^31 or greater than 2^31-1.
     **/
    public Interval shiftRight(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        if (r.from.compareTo(MIN_INT) < 0 || r.to.compareTo(MAX_INT) > 0) {
            return EXCEPTION;
        }

        final BigInteger s1 = from.shiftRight(r.from.intValue());
        final BigInteger s2 = from.shiftRight(r.to.intValue());
        final BigInteger s3 = to.shiftRight(r.from.intValue());
        final BigInteger s4 = to.shiftRight(r.to.intValue());
        return new Interval(min(s1, s2, s3, s4), max(s1, s2, s3, s4));
    }

    /**
     * Returns the interval containing the difference of an integer in this
     * interval minus an integer in interval <param>r</param>.
     *
     * @param r interval containing the integer to be subtracted
     * @return the interval containing the difference or <code>EXCEPTION</code>
     * if either interval is <code>EXCEPTION</code>
     **/
    public Interval subtract(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        return new Interval(from.subtract(r.to), to.subtract(r.from));
    }

    /**
     * Returns the interval containing the negative of an integer in this
     * interval.
     *
     * @return the interval containing the negation or <code>EXCEPTION</code>
     * if this interval is <code>EXCEPTION</code>
     **/
    public Interval negate() {
        if (checkException(this)) return EXCEPTION;
        return new Interval(to.negate(), from.negate());
    }

    private boolean strictlyPositive() {
        return to.signum() == 1 && from.signum() == 1;
    }

    private boolean zeroOrPositive() {
        return to.signum() >= 0 && from.signum() >= 0;
    }

    private boolean strictlyNegative() {
        return to.signum() == -1 && from.signum() == -1;
    }

    private boolean zeroOrNegative() {
        return to.signum() <= 0 && from.signum() <= 0;
    }

    /**
     * Returns the interval containing the bitwise AND of an integer in this
     * interval with an integer in interval <param>r</param>.
     *
     * @param r interval containing the integer to be AND'ed
     * @return the interval containing the AND result or <code>EXCEPTION</code>
     * if either interval is <code>EXCEPTION</code>
     **/
    public Interval and(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        final Interval lparts[] = partitionBySign();
        final Interval rparts[] = r.partitionBySign();
        final Interval result[] = new Interval[7];
        int k = 0;

        if (lparts[0] != null) {
            // only a negative number AND'ed with a negative number returns a
            // negative number, because negative numbers can be thought as
            // having an infinite number of sign bits
            if (rparts[0] != null) {
                result[k++] =
                    (new Interval(maxLength(lparts[0].from, rparts[0].from)))
                    .negate();
            }
            if (rparts[1] != null) {
                result[k++] = ZERO;
            }
            if (rparts[2] != null) {
                result[k++] =
                    new Interval(maxLength(lparts[0].from, rparts[2].to));
            }
        }
        if (lparts[1] != null) {
            result[k++] = ZERO;
        }
        if (lparts[2] != null) {
            if (rparts[0] != null) {
                result[k++] =
                    new Interval(maxLength(lparts[2].to, rparts[0].from));
            }
            if (rparts[1] != null) {
                result[k++] = ZERO;
            }
            if (rparts[2] != null) {
                result[k++] =
                    new Interval(maxLength(lparts[2].to, rparts[2].to));
            }
        }

        return union(result);
    }

    /**
     * Returns the interval containing the bitwise NOT of an integer in this
     * interval.
     *
     * @return the interval containing the bitwise not or
     * <code>EXCEPTION</code> if this interval is <code>EXCEPTION</code>
     **/
    public Interval not() {
        if (checkException(this)) return EXCEPTION;
        return new Interval(to.not(), from.not());
    }

    // Returns the maximum bit length of BigIntegers
    private int maxLength(final BigInteger... vals) {
        int result = 0;
        for (BigInteger val : vals) {
            result = Math.max(result, val.bitLength());
        }
        return result;
    }

    /**
     * Returns the interval containing the bitwise OR of an integer in this
     * interval with an integer in interval <param>r</param>.
     *
     * @param r interval containing the integer to be OR'ed
     * @return the interval containing the OR result or <code>EXCEPTION</code>
     * if either interval is <code>EXCEPTION</code>
     **/
    public Interval or(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        final Interval lparts[] = partitionBySign();
        final Interval rparts[] = r.partitionBySign();
        final Interval result[] = new Interval[7];
        int k = 0;

        if (lparts[0] != null) {
            if (rparts[0] != null) {
                result[k++] =
                    (new Interval(maxLength(lparts[0].from, rparts[0].from)))
                    .negate();
            }
            if (rparts[1] != null) {
                result[k++] = lparts[0];
            }
            if (rparts[2] != null) {
                result[k++] =
                    (new Interval(maxLength(lparts[0].from, rparts[2].to)))
                    .negate();
            }
        }
        if (lparts[1] != null) {
            result[k++] = r;
        }
        if (lparts[2] != null) {
            if (rparts[0] != null) {
                result[k++] =
                    (new Interval(maxLength(lparts[2].to, rparts[0].from)))
                    .negate();
            }
            if (rparts[1] != null) {
                result[k++] = lparts[2];
            }
            // only a positive number AND'ed with a positive number returns a
            // positive number, because negative numbers can be thought as
            // having an infinite number of sign bits
            if (rparts[2] != null) {
                result[k++] =
                    new Interval(maxLength(lparts[2].to, rparts[2].to));
            }
        }

        return union(result);
    }

    /**
     * Returns the interval containing the bitwise XOR of an integer in this
     * interval with an integer in interval <param>r</param>.
     *
     * @param r interval containing the integer to be XOR'ed
     * @return the interval containing the XOR result or <code>EXCEPTION</code>
     * if either interval is <code>EXCEPTION</code>
     **/
    public Interval xor(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        int max = maxLength(from, to, r.from, r.to);
        boolean negative = strictlyNegative() & r.strictlyPositive() | r.strictlyNegative() & strictlyPositive();
        boolean positive = zeroOrPositive() & r.zeroOrPositive() | strictlyNegative() & r.strictlyNegative();  

        if (negative) return new Interval(bitsInt(max,true), BigInteger.valueOf(-1));
        if (positive) return new Interval(max);
        return new Interval(true, max+1);
    }
    
    // Returns true if the interval contains odd numbers
    private boolean hasOdd() {
        final BigInteger two = BigInteger.valueOf(2);
        for (BigInteger i = from; i.compareTo(to) <= 0;
             i = i.add(BigInteger.ONE)) {
            if (i.mod(two).signum() != 0) return true;
        }
        return false;
    }

    // Returns true if the interval contains even numbers
    private boolean hasEven() {
        final BigInteger two = BigInteger.valueOf(2);
        for (BigInteger i = from; i.compareTo(to) <= 0;
             i = i.add(BigInteger.ONE)) {
            if (i.mod(two).signum() == 0) return true;
        }
        return false;
    }

    // Return base**exp, where base and exp are positive
    private Interval simplePow(final Interval base, final Interval exp) {
        assert exp.from.signum() == 1 && exp.to.signum() == 1 &&
               base.from.signum() == 1 && base.to.signum() == 1;
        if (exp.from.compareTo(MAX_INT) > 0 || exp.to.compareTo(MAX_INT) > 0) {
            return EXCEPTION;
        }
        final BigInteger p1 = base.to.pow(exp.to.intValue());
        final BigInteger p2 = base.from.pow(exp.from.intValue());
        return new Interval(min(p1, p2), max(p1, p2));
    }

    /**
     * Returns the interval containing an integer in this interval to the power
     * of an integer in interval <param>r</param>.
     *
     * @param r interval containing the exponent
     * @return the interval containing the result or <code>EXCEPTION</code> if
     * either interval is <code>EXCEPTION</code>
     **/
    public Interval pow(final Interval r) {
        if (checkException(this, r)) return EXCEPTION;
        final Interval[] base = partitionBySign();
        final Interval[] exp = r.partitionBySign();
        final Interval[] result = new Interval[11];
        int k = 0;
        if (exp[0] != null) {
            if (base[0] != null) {
                // a < 0, b < 0, a**b = 1/(a**(-b))
                if (base[0].to.equals(BigInteger.ONE.negate())) {
                    // if a = -1, a**b =  1 if b is even
                    //                 = -1 if b is odd
                    if (exp[0].hasOdd()) result[k++] = ONE.negate();
                    if (exp[0].hasEven()) result[k++] = ONE;
                }
                if (!base[0].from.equals(BigInteger.ONE.negate())) {
                    // if a < -1, a**b = 0
                    result[k++] = ZERO;
                }
            }

            // b < 0, 0**b = 1/(0**(-b)) = 1/0 = 0
            if (base[1] != null) result[k++] = ZERO;

            if (base[2] != null) {
                // a = 1, b < 0, a**b = 1
                if (base[2].from.equals(BigInteger.ONE)) {
                    result[k++] = ONE;
                }
                // a > 1, b < 0, a**b = 0
                if (!base[2].to.equals(BigInteger.ONE)) {
                    result[k++] = ZERO;
                }
            }
        }
        if (exp[1] != null) {
            // a**0 = 1 for all a
            result[k++] = ONE;
        }
        if (exp[2] != null) {
            if (base[0] != null) {
                // a < 0, b > 0, calculate (-a)**b, then calculate the sign
                // depending on odd/even power
                final Interval x = simplePow(base[0].negate(), exp[2]);
                if (exp[2].hasOdd()) result[k++] = x.negate();
                if (exp[2].hasEven()) result[k++] = x;
            }
            // b > 0, 0**b = 0
            if (base[1] != null) result[k++] = ZERO;
            // a > 0, b > 0
            if (base[2] != null) {
                result[k++] = simplePow(base[2], exp[2]);
            }
        }
        return union(result);
    }

    /**
     * Returns an interval containing the value of the bits extracted from an
     * integer in this interval starting at a bit position from interval
     * <code>min</code> and ending at a bit position from interval
     * <code>max</code>.
     *
     * @param min interval containing the starting bit position
     * @param max interval containing the ending bit position
     * @return the interval containing the extracted bits or
     * <code>EXCEPTION</code> if any input interval is <code>EXCEPTION</code>
     **/
    public Interval bitExtract(final Interval min, final Interval max) {
        if (checkException(this, min, max)) return EXCEPTION;

        // determine the maximum number of bits that can be extracted, and
        // assuming the result can be any integer representable in that bit
        // width
        final Interval diff = max.subtract(min).add(ONE);
        if (diff.to.compareTo(MAX_INT) > 0) {
            return EXCEPTION;
        }

        return new Interval(Math.max(1, diff.to.intValue()));
    }

    private String toString(BigInteger x) {
        final int width = x.bitLength();
        final String result;
        if (width > 32) {
            result = (x.signum() == -1 ? "-" : "") + "2**" + width;
        } else {
            result = x.toString();
        }
        return result;
    }

    public String toString() {
        if (this == EXCEPTION) {
            return "[EXCEPTION]";
        } else {
            return "[" + toString(from) + ".." + toString(to) + "]";
        }
    }
}
