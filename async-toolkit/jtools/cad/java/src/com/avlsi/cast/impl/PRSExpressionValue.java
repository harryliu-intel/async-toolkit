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

package com.avlsi.cast.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.avlsi.cell.CellImpl;
import com.avlsi.file.common.HierName;
import com.avlsi.util.bool.AndBooleanExpression;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpression;
import com.avlsi.util.exception.AssertionFailure;

/**
 * Class to build up expressions for use in productions rules. 
 * The left hand side of a production rules corresponds to a
 * <code>PRSExpressionValue</code>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class PRSExpressionValue extends Value {

    private final BooleanExpressionInterface be;

    /**
     * Class constructor.
     **/
    public PRSExpressionValue(final BooleanExpressionInterface be) {
        super(true);

        this.be = be;
    }

    /**
     * Class constructor.
     **/
    public PRSExpressionValue(final HierName hn) {
        this(new HierNameAtomicBooleanExpression(true, hn));
    }

    public Value duplicate() {
        throw new AssertionFailure("Don't call me!");
    }

    /**
     * Converts a PRSExpressionValue or NodeValue to a PRSExpressionValue,
     * throws InvalidOperationException if the Value is not one of 
     * these subclasses.
     **/
    public static PRSExpressionValue valueOf(Value v)
        throws InvalidOperationException {
        if (v instanceof PRSExpressionValue)
            return (PRSExpressionValue) v;
        else if (v instanceof NodeValue) {
            final NodeValue nv = (NodeValue) v;
            return new PRSExpressionValue(
                    new HierNameAtomicBooleanExpression(
                        true, nv.getInstanceName()));
        } else
            throw new InvalidOperationException("Cannot convert type "
                    + v.getClass().getName() + " to prs expression.");
    }

    /**
     * Makes a collection from two BooleanExpressionInterfaces, 
     * for use by <code>or</code> and <code>and</code>.
     **/
    private Collection booleanExpressionsToCollection(
            final BooleanExpressionInterface be1,
            final BooleanExpressionInterface be2) {
        final List l = new ArrayList(2);
        l.add(be1);
        l.add(be2);
        return l;
    }

    // logical
    public Value or       (final Value v) throws InvalidOperationException {
        final PRSExpressionValue prsExprVal = valueOf(v);
        return new PRSExpressionValue(
                new OrBooleanExpression(
                    true,
                    booleanExpressionsToCollection(be, prsExprVal.be)));
    }

    public Value and      (final Value v) throws InvalidOperationException {
        final PRSExpressionValue prsExprVal = valueOf(v);
        return new PRSExpressionValue(
                new AndBooleanExpression(
                    true,
                    booleanExpressionsToCollection(be, prsExprVal.be)));
    }

    public Value not      ()              throws InvalidOperationException {
        return new PRSExpressionValue(be.negated());
    }

    public Value assign   (final Value v, final CellImpl cell) {
        throw new AssertionFailure("Don't call me!");
    }

    public Type getType() {
        throw new AssertionFailure("Don't call me!");
    }

    public BooleanExpressionInterface getBooleanExpression() {
        return be;
    }
}
