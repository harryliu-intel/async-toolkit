/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public class CElementExpression extends ShorthandNodeExpression {
    public CElementExpression(NodeName a, NodeName b, boolean reset) {
        super(new NodeName[] { a, b }, reset);
    }

    protected String operator(int i, Direction preferredDirection) {
        switch (i) {
        case 0:
            return "&";
        case 1:
            return "#>";
        }
        throw new RuntimeException();
    }

    public NodeExpression unparameterize(int index) {
        return new CElementExpression(names[0].unparameterize(index),
                                      names[1].unparameterize(index),
                                      reset);
    }

    public NodeExpression canonicalize(NodeCanonicalizer c) {
        return new CElementExpression(c.canonicalize(names[0]),
                                      c.canonicalize(names[1]),
                                      reset);
    }

    public NodeExpression applySimplification(Simplification simp) {
        SHSimplification shs = (SHSimplification) simp;
        boolean hasGND = shs.isGND(0) || shs.isGND(1);
        boolean hasVdd = shs.isVdd(0) || shs.isVdd(1);

        if (!simp.isSimpler())
            return this;

        if (reset) {
            if (hasVdd)
                return new NandExpression(new NodeName[]
                    { new UnparameterizedNodeName("_Reset") }, false);
            else
                return new AliasNodeExpression(Power.Vdd, 0);
        }

        if (hasGND && !hasVdd) {
            return new AliasNodeExpression(Power.Vdd, 0);
        } else if (hasVdd && !hasGND) {
            return new AliasNodeExpression(Power.GND, 0);
        }

        throw new RuntimeException("C Element has one input tied to GND " +
                                   "and one input tied to Vdd.  This is bad!");
    }
}
