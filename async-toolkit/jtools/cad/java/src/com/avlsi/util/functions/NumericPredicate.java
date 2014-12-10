package com.avlsi.util.functions;

/**
 * Defines <code>BinaryPredicate</code>s <code>EQ</code>, <code>LT</code>,
 * <code>GT</code>, <code>GE</code>, and <code>LE</code> corresponding to
 * equals, less than, greater than, greater or equals, and less or equals, on
 * <code>Number</code> arguments, respectively.
 **/
public class NumericPredicate {
    private static BinaryPredicate relation(final int lower,
                                            final int upper) {
        return new BinaryPredicate() {
            public boolean evaluate(final Object a, final Object b) {
                final int c = Double.compare(((Number) a).doubleValue(),
                                             ((Number) b).doubleValue());
                return lower <= c && c <= upper;
            }
        };
    }
    public static final BinaryPredicate EQ = relation(0, 0);
    public static final BinaryPredicate NE = new BinaryPredicate.Not(EQ);
    public static final BinaryPredicate LT = relation(Integer.MIN_VALUE, -1);
    public static final BinaryPredicate GT = relation(1, Integer.MAX_VALUE);
    public static final BinaryPredicate LE = relation(Integer.MIN_VALUE, 0);
    public static final BinaryPredicate GE = relation(0, Integer.MAX_VALUE);

    public static BinaryPredicate getPredicate(final String op) {
        final BinaryPredicate p;
        if      (op.equals("==")) p = NumericPredicate.EQ;
        else if (op.equals("!=")) p = NumericPredicate.NE;
        else if (op.equals("<"))  p = NumericPredicate.LT;
        else if (op.equals("<=")) p = NumericPredicate.LE;
        else if (op.equals(">"))  p = NumericPredicate.GT;
        else if (op.equals(">=")) p = NumericPredicate.GE;
        else throw new
            IllegalArgumentException("Invalid relational operator: " + op);
        return p;
    }

    /**
     * This class cannot be constructed.
     **/
    private NumericPredicate() { }
}
