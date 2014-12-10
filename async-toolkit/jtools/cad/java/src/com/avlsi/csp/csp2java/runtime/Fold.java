/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;

public abstract class Fold {
    public static final BinaryFunction ADD_FUNCTION =
        new AddFunction();
    public static final BinaryFunction SUBTRACT_FUNCTION =
        new SubtractFunction();
    public static final BinaryFunction AND_FUNCTION =
        new AndFunction();
    public static final BinaryFunction OR_FUNCTION =
        new OrFunction();
    public static final BinaryFunction XOR_FUNCTION =
        new XorFunction();
    public static final BinaryFunction MULTIPLY_FUNCTION =
        new MultiplyFunction();
    public static final BinaryFunction DIVIDE_FUNCTION =
        new DivideFunction();
    public static final BinaryFunction REMAINDER_FUNCTION =
        new RemainderFunction();
    public static final BinaryFunction LEFTSHIFT_FUNCTION =
        new LeftShiftFunction();
    public static final BinaryFunction RIGHTSHIFT_FUNCTION =
        new RightShiftFunction();

    public interface BinaryFunction {
        CspValue evaluate(CspValue a1, CspValue a2);
    }

    private static class AddFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            if (a1 instanceof CspString && a2 instanceof CspString) {
                return ((CspString) a1).add((CspString) a2);
            } else {
                return ((CspInteger) a1).add((CspInteger) a2);
            }
        }
    }
    private static class SubtractFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).subtract((CspInteger) a2);
        }
    }
    private static class AndFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).and((CspInteger) a2);
        }
    }
    private static class OrFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).or((CspInteger) a2);
        }
    }
    private static class XorFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).xor((CspInteger) a2);
        }
    }
    private static class MultiplyFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).multiply((CspInteger) a2);
        }
    }
    private static class DivideFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).divide((CspInteger) a2);
        }
    }
    private static class RemainderFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).remainder((CspInteger) a2);
        }
    }
    private static class LeftShiftFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).shiftLeft((CspInteger) a2);
        }
    }
    private static class RightShiftFunction implements BinaryFunction {
        public CspValue evaluate(CspValue a1, CspValue a2) {
            return ((CspInteger) a1).shiftRight((CspInteger) a2);
        }
    }

    public abstract CspValue evaluate(CspInteger i)
        throws InterruptedException;

    public CspValue fold(CspValue unit, BinaryFunction f,
                         CspInteger min, CspInteger max)
        throws InterruptedException {
        CspValue v = unit;
        for (CspInteger i = min; i.compareTo(max) <= 0;
                i = i.add(new CspInteger(BigInteger.ONE))) {
            v = f.evaluate(v, evaluate(i));
        }
        return v;
    }

    public static void main(String[] args) throws InterruptedException {
        System.err.println(
            new Fold() {
                public CspValue evaluate(final CspInteger i0)
                throws InterruptedException {
                    return new Fold() {
                        public CspValue evaluate(final CspInteger i1)
                        throws InterruptedException {
                // System.err.println("multiplying " + i0 + " * " + i1);
                            return i0.multiply(i1);
                        }
                    }.fold(new CspInteger(BigInteger.ZERO), ADD_FUNCTION,
                        new CspInteger(BigInteger.ZERO),
                        new CspInteger(new BigInteger("10")));
                }
            }.fold(new CspInteger(BigInteger.ZERO), ADD_FUNCTION,
              new CspInteger(BigInteger.ZERO),
              new CspInteger(new BigInteger("5")))
        );

        int n = 0;
        for (int i = 0; i <= 5; ++i)
            for (int j = 0; j <= 10; ++j)
                n += i * j;
        System.err.println(n);
    }
}
