package com.avlsi.csp.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.grammar.ParsePosition;
import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.util.container.Pair;

public class ArithmeticOperator implements Comparable<ArithmeticOperator> {
    private static final Pattern OPERATOR =
        Pattern.compile(".*\\.([^.]+)Expression");
    private final ExpressionInterface e;
    private final IntegerType ty;
    public ArithmeticOperator(final ExpressionInterface e,
                              final IntegerType ty) {
        this.e = e;
        this.ty = ty;
    }
    public int getWidth(final int defWidth) {
        return CspUtils.getWidth(ty.getInterval(), defWidth);
    }
    public String getOp() {
        if (e instanceof AbstractBinaryExpression) {
            return ((AbstractBinaryExpression) e).getOperator();
        } else {
            final Matcher m = OPERATOR.matcher(e.getClass().getName());
            if (m.matches()) {
                return m.group(1).toLowerCase();
            }
        }
        return "unknown operator";

    }
    public ParseRange getParseRange() {
        return e.getParseRange();
    }
    public boolean equals(Object x) {
        if (x instanceof ArithmeticOperator)
            return compareTo((ArithmeticOperator) x) == 0;
        else
            return false;
    }
    public int hashCode() {
        return e.hashCode() + ty.hashCode();
    }
    public int compareTo(final ArithmeticOperator other) {
        // compare based on start parse position
        int result;
        final ParsePosition astart = e.getParseRange().start;
        final ParsePosition bstart = other.e.getParseRange().start;

        result = astart.filename.compareTo(bstart.filename);
        if (result != 0) return result;

        result = astart.compareTo(bstart);
        if (result != 0) return result;
        else return getOp().compareTo(other.getOp());
    }
}
