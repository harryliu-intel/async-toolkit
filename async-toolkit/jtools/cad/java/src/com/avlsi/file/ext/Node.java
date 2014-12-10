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

package com.avlsi.file.ext;

import com.avlsi.file.common.HierName;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

/**
 * Class to represent an electrical node in a .ext file.
 * Lumped resistance has been ignored, as it is useless.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Node {
    private final HierName name;
    private final double capacitanceToGround;
    private final double[] resistClassPerims;
    private final double[] resistClassAreas;

    public Node(final HierName name,
                final double capacitanceToGround,
                final double[] resistClassPerims,
                final double[] resistClassAreas)
    {
        this.name = name;
        this.capacitanceToGround = capacitanceToGround;
        this.resistClassPerims = resistClassPerims;
        this.resistClassAreas = resistClassAreas;

        Debug.assertTrue(name != null);
        Debug.assertTrue(resistClassPerims != null);
        Debug.assertTrue(resistClassAreas != null);
    }

    /**
     * @review XXX should this always return a fresh object?
     **/
    public static final class NodeMergeFunction
            implements AliasedMap.MergeFunction {
        public Object merge(final Object o1, final Object o2)
            throws AliasedMap.MergeFailedException
        {
            if (o1 == null)
                return o2;
            else  if (o2 == null)
                return o1;
            else {
                final Node n1 = (Node) o1;
                final Node n2 = (Node) o2;

                // if names do not agree (merge of label-connected nodes)
                // then pick the more canonical name
                // even better: make the name non-intrinsic

                // cap
                final double c = n1.getCapacitanceToGround()
                    + n2.getCapacitanceToGround();

                // perims
                final double[] rcp1s = n1.getResistClassPerims();
                final double[] rcp2s = n2.getResistClassPerims();

                if (rcp1s.length != rcp2s.length)
                    throw new AliasedMap.MergeFailedException(
                            "Num resist class perims differ");

                final double[] rcps = new double[rcp1s.length];

                for (int i = 0; i < rcps.length; ++i) 
                    rcps[i] = rcp1s[i] + rcp2s[i];

                // areas
                final double[] rca1s = n1.getResistClassAreas();
                final double[] rca2s = n2.getResistClassAreas();

                if (rca1s.length != rca2s.length)
                    throw new AliasedMap.MergeFailedException(
                            "Num resist class areas differ");

                final double[] rcas = new double[rca1s.length];

                for (int i = 0; i < rcas.length; ++i) 
                    rcas[i] = rca1s[i] + rca2s[i];

                return new Node(n1.getName(), c, rcps, rcas);
            }
        }
    }

    /**
     * returns the name of the node
     **/
    public HierName getName() {
        return name;
    }

    // total capacitance to ground in attofarads
    public double getCapacitanceToGround() {
        return capacitanceToGround;
    }

    public double[] getResistClassPerims() {
        return resistClassPerims;
    }
    public double[] getResistClassAreas() {
        return resistClassAreas;
    }
    public double getAreaForResistClass(int i) {
        return resistClassAreas[i];
    }
    public double getPerimForResistClass(int i) {
        return resistClassPerims[i];
    }

    public String toString() {
        return "Node[" + getName().toString() + "]";
    }
}
