/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import com.avlsi.tools.presto.ChannelName;
import com.avlsi.tools.synthesis.Espresso;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import com.avlsi.tools.presto.output.NodeName;
import com.avlsi.tools.presto.TruthTable;
import com.avlsi.tools.presto.output.UnparameterizedNodeName;

public class DoLogic {
    private DoLogic() {}        // static only

    /**
     * Takes the specified rail of the given TruthTable, and returns the
     * minimized DNF for its pulldowns, suitable for passing to the
     * downs parameter of the ArbitraryNodeExpression constructor.
     * @param  tt          the TruthTable to use
     * @param  rail        the rail of the TruthTable to look at
     *                     (just as in the TruthTable interface, -1 means
     *                     the "no output" rail)
     * @param  boilerplate nodes to always put in series with each disjunction
     *                     (i. e. go or enables)
     * @param  dependOn    on return, a bit will be set in this BitSet for
     *                     each input that this output depends on
     * @return an array (representing disjunctions) of arrays (representing
     *         conjunctions) of nodes
     */
    public static NodeName[][] doLogic(TruthTable tt, int rail,
                                       NodeName[] boilerplate,
                                       BitSet dependOn) {
        ArrayList exclusives = new ArrayList();
        LinkedHashSet rules = new LinkedHashSet();

        ChannelName[] inputs = tt.getInputs();
        byte[] table = tt.getTable();

        /* compute "values" for each input channel (thinking of table
         * as being indexed by a multidigit number with varying base) */
        int value[] = new int[inputs.length];
        int v = 1;
        for (int i = 0; i < value.length; i++) {
            value[i] = v;
            v *= inputs[i].get1of();
        }

        // detect inputs that don't matter
        boolean important[] = new boolean[inputs.length];
        for (int i = 0; i < important.length; i++)
            important[i] = false;
        for (int i = 0; i < table.length; i++)
            for (int j = 0; j < important.length; j++) {
                int of = inputs[j].get1of();
                int extractedInputIndex = (i / value[j]) % of;
                int newInputIndex = (extractedInputIndex + 1) % of;
                int i2 = (i - (extractedInputIndex * value[j]) +
                          (newInputIndex * value[j]));
                if ((table[i] == rail) != (table[i2] == rail))
                    important[j] = true;
            }

        // (now copy that into the dependOn BitSet)
        for (int i = 0; i < important.length; i++)
            if (important[i])
                dependOn.set(i);

        // create exclusives
        String[][] inputRails = new String[inputs.length][];
        for (int i = 0; i < inputs.length; i++) {
            int n = inputs[i].get1of();
            if (important[i]) {
                String chname = inputs[i].getNameWithIndices();
                String[] rails = new String[n];
                for (int j = 0; j < n; j++)
                    rails[j] = chname + "." + j;
                if (n > 1)
                    exclusives.add(Arrays.asList(rails));
                inputRails[i] = rails;
            }
        }

        // create rules
        for (int i = 0; i < table.length; i++) 
            if (table[i] == rail) {
                List nodes = new ArrayList();
                int x = i;
                for (int j = 0; j < inputs.length; j++) {
                    int n = inputs[j].get1of();
                    String[] rails = inputRails[j];
                    if (important[j])
                        nodes.add(rails[x % n]);
                    x /= n;
                }
                rules.add(nodes);
            }

        // minimize
        List optimized =
            new Espresso(exclusives, new ArrayList(rules)).optimizedRules();

        // convert result
        NodeName[][] result = new NodeName[optimized.size()][];
        int i = 0;
        for (Iterator it = optimized.iterator(); it.hasNext(); i++) {
            List rule = (List) it.next();
            NodeName[] nodes = new NodeName[rule.size() + boilerplate.length];
            int j;
            for (j = 0; j < boilerplate.length; j++)
                nodes[j] = boilerplate[j];
            for (Iterator it2 = rule.iterator(); it2.hasNext(); j++)
                nodes[j] = new UnparameterizedNodeName((String)it2.next());
            result[i] = nodes;
        }

        return result;
    }
}
