/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public class TwoAnd2C2Expression extends ShorthandNodeExpression {
    public TwoAnd2C2Expression(NodeName a1, NodeName a2,
                               NodeName b1, NodeName b2, boolean reset) {
        super(new NodeName[] { a1, a2, b1, b2 }, reset);
    }

    protected String operator(int i, Direction preferredDirection) {
        switch (i) {
        case 0:
        case 2:
            return "|";
        case 1:
            return "&";
        case 3:
            return "#>";
        }
        throw new RuntimeException();
    }

    // 2and2c2 can only be written in the "up" direction
    protected Direction overrideDirection(Direction preferredDirection) {
        return Direction.UP;
    }

    protected boolean parenBefore(int i) {
        return ((i & 1) == 0);
    }

    protected boolean parenAfter(int i) {
        return ((i & 1) == 1);
    }

    public NodeExpression unparameterize(int index) {
        return new TwoAnd2C2Expression(names[0].unparameterize(index),
                                       names[1].unparameterize(index),
                                       names[2].unparameterize(index),
                                       names[3].unparameterize(index),
                                       reset);
    }

    public NodeExpression canonicalize(NodeCanonicalizer c) {
        return new TwoAnd2C2Expression(c.canonicalize(names[0]),
                                       c.canonicalize(names[1]),
                                       c.canonicalize(names[2]),
                                       c.canonicalize(names[3]),
                                       reset);
    }

    public NodeExpression applySimplification(Simplification simp) {
        SHSimplification shs = (SHSimplification) simp;
        boolean hasGND =
            shs.isGND(0) || shs.isGND(1) || shs.isGND(2) || shs.isGND(3);
        boolean hasVdd =
            (shs.isVdd(0) && shs.isVdd(1)) || (shs.isVdd(2) && shs.isVdd(3));

        if (!simp.isSimpler())
            return this;

        if (reset) {
            if (hasVdd)
                return new NandExpression(new NodeName[]
                    { new UnparameterizedNodeName("_Reset") }, false);
            else if (hasGND)
                return new AliasNodeExpression(Power.Vdd, 0);
        }

        if (hasGND && !hasVdd) {
            return new AliasNodeExpression(Power.Vdd, 0);
        } else if (hasVdd && !hasGND) {
            return new AliasNodeExpression(Power.GND, 0);
        } else if (hasGND && hasVdd) {
            throw new RuntimeException("C Element has one input tied to GND " +
                                       "and one input tied to Vdd.  " +
                                       "This is bad!");
        }

        NodeName n1 = null, n2 = null;

        if (shs.isVdd(0)) {
            n1 = names[1];
        } else if (shs.isVdd(1)) {
            n1 = names[0];
        }

        if (shs.isVdd(2)) {
            n2 = names[3];
        } else if (shs.isVdd(3)) {
            n2 = names[2];
        }

        if (n1 != null && n2 != null)
            return new CElementExpression(n1, n2, reset);

        if (n1 == null) {
            assert (n2 != null);
            return new And2C2Expression(names[0], names[1], n2, reset);
        } else {
            assert (n2 == null);
            return new And2C2Expression(names[2], names[3], n1, reset);
        }
    }
}
