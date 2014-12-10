/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public class And2C2Expression extends ShorthandNodeExpression {
    public And2C2Expression(NodeName a1, NodeName a2, NodeName b, boolean reset) {
        super(new NodeName[] { a1, a2, b }, reset);
    }

    protected String operator(int i, Direction preferredDirection) {
        switch (i) {
        case 0:
            return "|";
        case 1:
            return "&";
        case 2:
            return "#>";
        }
        throw new RuntimeException();
    }

    // and2c2 can only be written in the "up" direction
    protected Direction overrideDirection(Direction preferredDirection) {
        return Direction.UP;
    }

    protected boolean parenBefore(int i) {
        return (i == 0);
    }

    protected boolean parenAfter(int i) {
        return (i == 1);
    }

    public NodeExpression unparameterize(int index) {
        return new And2C2Expression(names[0].unparameterize(index),
                                    names[1].unparameterize(index),
                                    names[2].unparameterize(index),
                                    reset);
    }

    public NodeExpression canonicalize(NodeCanonicalizer c) {
        return new And2C2Expression(c.canonicalize(names[0]),
                                    c.canonicalize(names[1]),
                                    c.canonicalize(names[2]),
                                    reset);
    }

    public NodeExpression applySimplification(Simplification simp) {
        SHSimplification shs = (SHSimplification) simp;
        boolean hasGND = shs.isGND(0) || shs.isGND(1) || shs.isGND(2);
        boolean hasVdd = (shs.isVdd(0) && shs.isVdd(1)) || shs.isVdd(2);

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

        NodeName n;

        if (shs.isVdd(0)) {
            assert (!shs.isVdd(1));
            n = names[1];
        } else {
            assert (shs.isVdd(1));
            n = names[0];
        }

        return new CElementExpression(n, names[2], reset);
    }
}
