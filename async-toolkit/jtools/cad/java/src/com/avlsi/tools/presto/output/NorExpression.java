/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

import java.util.LinkedList;

public class NorExpression extends ShorthandNodeExpression {
    public NorExpression(NodeName[] names, boolean reset) {
        super(names, reset);
    }

    protected String operator(int i, Direction preferredDirection) {
        if (i == names.length - 1)
            return "=>";
        return (preferredDirection == Direction.UP ? "&" : "|");
    }

    public NodeExpression unparameterize(int index) {
        NodeName[] newnames = new NodeName[names.length];
        for (int i = 0; i < newnames.length; i++)
            newnames[i] = names[i].unparameterize(index);
        return new NorExpression(newnames, reset);
    }

    public NodeExpression canonicalize(NodeCanonicalizer c) {
        NodeName[] newnames = new NodeName[names.length];
        for (int i = 0; i < newnames.length; i++)
            newnames[i] = c.canonicalize(names[i]);
        return new NorExpression(newnames, reset);
    }

    public NodeExpression applySimplification(Simplification simp) {
        SHSimplification shs = (SHSimplification) simp;

        if (!simp.isSimpler())
            return this;

        LinkedList newnodes = new LinkedList();
        for (int i = 0; i < names.length; i++) {
            if (shs.isVdd(i)) {
                return new AliasNodeExpression(Power.GND, 0);
            } else if (!shs.isGND(i)) {
                newnodes.add(names[i]);
            }
        }

        boolean rst = reset;

        if (newnodes.size() == 0 && rst) {
            newnodes.add(new UnparameterizedNodeName("_Reset"));
            rst = false;
        }

        if (newnodes.size() == 0) {
            return new AliasNodeExpression(Power.Vdd, 0);
        }
        
        return new NorExpression((NodeName[])
                                 newnodes.toArray(new NodeName[0]), rst);
    }
}
