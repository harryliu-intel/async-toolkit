/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * Loop expressions, <code>&lt; op i : M..N : expr &gt;</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class LoopExpression extends AbstractLoop
    implements ExpressionInterface {

    /** Bitwise and separator constant.  **/
    public static final int AND = 0;

    /** Bitwise or separator constant.  **/
    public static final int OR = 1;

    /** Multiplication separator constant.  **/
    public static final int TIMES = 2;

    /** Addition separator constant.  **/
    public static final int PLUS = 3;

    /** Bitwise xor separator constant.  **/
    public static final int XOR = 4;

    /** Loop body expression, not null.  **/
    private final ExpressionInterface expr;

    /**
     * Class constructor.
     *
     * @param indexVar loop index variable, not null
     * @param range minimum and maximum of range for loop index, not null
     * @param separator  separator code, one of {@see #AND}, {@see #OR},
     *     {@see #TIMES}, {@see #PLUS}
     * @param expr loop body expression
     **/
    public LoopExpression(final String indexVar,
            final Range range,
            final int separator,
            final ExpressionInterface expr) {
        super(indexVar, range, separator);
        this.expr = expr;
    }

    /**
     * Returns body expression.
     *
     * @return body expression, not null
     **/
    public ExpressionInterface getExpression() {
        return expr;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLoopExpression(this);
    }

    public String getSeparatorString() {
        final int sep = getSeparator();
        switch(sep) {
          case AND:   return "&";
          case OR:    return "|";
          case XOR:   return "^";
          case TIMES: return "*";
          case PLUS:  return "+";
        }
        throw new AssertionError("Unknown separator: " + sep);
    }
}
