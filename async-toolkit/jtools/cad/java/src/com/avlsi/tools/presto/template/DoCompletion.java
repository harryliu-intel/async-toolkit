/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import com.avlsi.tools.presto.output.And2C2Expression;
import java.util.Arrays;
import java.util.BitSet;
import com.avlsi.tools.presto.output.CElementExpression;
import com.avlsi.tools.presto.ChannelName;
import java.util.Collection;
import com.avlsi.tools.presto.output.Direction;
import com.avlsi.tools.presto.complete.GreedyComplete;
import java.util.Iterator;
import java.util.Map;
import com.avlsi.tools.presto.output.NandExpression;
import com.avlsi.tools.presto.complete.Node;
import com.avlsi.tools.presto.complete.NodeBuilder;
import com.avlsi.tools.presto.output.NodeExpression;
import com.avlsi.tools.presto.output.NodeName;
import com.avlsi.tools.presto.output.NorExpression;
import com.avlsi.tools.presto.output.Section;
import java.util.TreeMap;
import com.avlsi.tools.presto.complete.TreeMunger;
import com.avlsi.tools.presto.complete.TreeReadjuster;
import com.avlsi.tools.presto.output.TwoAnd2C2Expression;
import com.avlsi.tools.presto.output.UnparameterizedNodeName;
import com.avlsi.tools.presto.output.WholeOperator;
import com.avlsi.tools.presto.output.WholeOperatorSink;

public class DoCompletion {
    private DoCompletion() {}        // static only

    private static Node mungeTree(Node root, final Map temporaryNames,
                                  final String rootName,
                                  final int[] rootIndex) {
        final int rootTier = root.getTier();
        return TreeMunger.mungeTree(root, new NodeBuilder() {
                public Node buildNode(int type, int tier, Node[] children) {
                    String nameOverride = (tier == rootTier ? rootName : null);
                    return Doodad.conjugate(type, tier, children,
                                            temporaryNames, nameOverride,
                                            rootIndex);
                }
            });
    }

    /**
     * Take a completion tree in which the intermediate nodes are
     * unnamed, and return an identical completion tree in which they
     * are named.  All of the code that really does this is in the
     * Doodad class.
     */
    public static Node assignNames(Node root, String rootName,
                                   int[] rootIndex) {
        // dry run, to figure out which temporaries need to be arrays
        Map temporaryNames = new TreeMap();
        mungeTree(root, temporaryNames, rootName, rootIndex);

        /* Remove everything from the Map, except leave a zero-length
         * array for the temporaries that don't need to be an array. */
        for (Iterator it = temporaryNames.entrySet().iterator();
             it.hasNext(); ) {
            Map.Entry e = (Map.Entry) it.next();
            int[] a = (int[]) e.getValue();
            assert (a.length == 1);
            assert (a[0] > 0);
            if (a[0] == 1)
                e.setValue(new int[0]);
            else
                it.remove();
        }

        // now do it for real
        return mungeTree(root, temporaryNames, rootName, rootIndex);
    }

    /**
     * Same as above, but does multiple, separate completion trees at
     * once.  (The point of this is to share the generated node-name
     * space across all completion trees.)
     */
    public static Node[] assignNames(Node[] root, String[] rootName) {
        assert (root.length == rootName.length);
        // dry run, to figure out which temporaries need to be arrays
        Map temporaryNames = new TreeMap();
        for (int i = 0; i < root.length; i++)
            mungeTree(root[i], temporaryNames, rootName[i], new int[0]);

        /* Remove everything from the Map, except leave a zero-length
         * array for the temporaries that don't need to be an array. */
        for (Iterator it = temporaryNames.entrySet().iterator();
             it.hasNext(); ) {
            Map.Entry e = (Map.Entry) it.next();
            int[] a = (int[]) e.getValue();
            assert (a.length == 1);
            assert (a[0] > 0);
            if (a[0] == 1)
                e.setValue(new int[0]);
            else
                it.remove();
        }

        // now do it for real
        Node[] result = new Node[root.length];
        for (int i = 0; i < root.length; i++)
            result[i] = mungeTree(root[i], temporaryNames, rootName[i],
                                  new int[0]);
        return result;
    }

    /**
     * Returns a tree which is like the given tree, except that inverters
     * in the input completion portion of the tree are shifted as close
     * to the root as possible.  This is to avoid dumb stuff like this:
     * L.0 => _L0-                                   // tier 1
     * L.1 => _L1-
     * L.2 => _L2-
     * L.3 => _L3-
     * ~_L0 | ~_L1 => L01+                           // tier 2
     * ~_L2 | ~_L3 => L23+
     * L01 | L23 => _Lv-                             // tier 3
     */
    public static Node shiftInvertersUp(Node root) {
        return TreeMunger.mungeTree(root, new NodeBuilder() {
                private boolean isInput(Node n) {
                    Doodad doodad = (Doodad) n.getCookie();
                    if (doodad == null) {
                        for (int i = 0; i < n.size(); i++)
                            if (!isInput(n.get(i)))
                                return false;
                        return true;
                    } else {
                        return (doodad.sectType == Doodad.IN);
                    }
                }

                public Node buildNode(int type, int tier, Node[] children) {
                    if (children.length == 2 && children[0].size() == 1 &&
                        children[1].size() == 1 && isInput(children[0]) &&
                        isInput(children[1])) {
                        Node prev = new Node(type, null, null, tier - 1, null,
                                             new Node[] { children[0].get(0),
                                                          children[1].get(0)});
                        return new Node(children[0].getType(), null, null,
                                        tier, null, new Node[] { prev });
                    } else {
                        return new Node(type, null, null, tier,
                                        null, children);
                    }
                }
            });
    }

    /**
     * Creates an array of nodes that represent the nodes in the
     * given channel.
     * @param channel  the channel
     * @param tier     which tier (usually 0 for inputs and 1 for outputs)
     *                 these nodes start at
     * @param sectType one of Doodad.IN, Doodad.OUT, or Doodad.BOTH
     * @param rails    Collection of Numbers: the rails in channel which matter
     */
    public static Node[] nodesForChannel(ChannelName channel,
                                         int tier, int sectType,
                                         Collection rails) {
        Node[] result = new Node[rails.size()];
        int i = 0;
        for (Iterator it = rails.iterator(); it.hasNext(); i++) {
            int rail = ((Number)it.next()).intValue();
            BitSet railSet = new BitSet();
            railSet.set(rail);
            // leaves don't need a section
            Doodad doodad = new Doodad(null, sectType, channel,
                                       railSet, rails.size());
            result[i] = new Node(channel.getName(), channel.getArrayIndices(),
                                 rail, tier, doodad);
        }
        return result;
    }
    
    /**
     * As above, but simply assumes all rails in the channel are used.
     */
    public static Node[] nodesForChannel(ChannelName channel,
                                         int tier, int sectType) {
        Integer[] a = new Integer[channel.get1of()];
        for (int i = 0; i < a.length; i++)
            a[i] = new Integer(i);
        return nodesForChannel(channel, tier, sectType, Arrays.asList(a));
    }

    /**
     * Take a Node of arbitrary type, and create an identical Node
     * of type TERMINAL.
     */
    public static Node makeTerminal(Node n) {
        return new Node(n.getName(), n.getArrayIndices(), n.getRail(),
                        n.getTier(), n.getCookie());
    }

    /**
     * So, maybe I made a mistake by using two completely different
     * data structures to represent a node.  It seemed to make sense
     * at the time.  Anyway, this function converts a Node to a
     * NodeName.
     */
    public static NodeName nodeToNodeName(Node n) {
        StringBuffer buf = new StringBuffer(n.getName());
        int[] ai = n.getArrayIndices();

        if (ai.length > 0) {
            char c = '[';
            for (int i = 0; i < ai.length; i++) {
                buf.append(c);
                buf.append(Integer.toString(ai[i]));
                c = ',';
            }
            buf.append(']');
        }

        if (n.getRail() != Node.NO_RAIL) {
            buf.append('.');
            buf.append(Integer.toString(n.getRail()));
        }

        return new UnparameterizedNodeName(buf.toString());
    }

    /**
     * Copies a completion tree to a WholeOperatorSink and a
     * Declarations.
     * @param  root  the root node of the completion tree
     * @param  wos    the final completion tree is output here
     * @param  decl   intermediate nodes are declared here
     * @param  reset  true means add _Reset to the root node
     */
    public static void emitTree(Node root, WholeOperatorSink wos,
                                Declarations decl, boolean reset) {
        NodeName dest = nodeToNodeName(root);
        NodeName[] names = new NodeName[root.size()];
        for (int i = 0; i < root.size(); i++)
            names[i] = nodeToNodeName(root.get(i));

        NodeExpression nex = null;

        switch (root.getType()) {
        case Node.NOR:
            if (root.getSense() == Node.ACTIVE_LOW)
                nex = new NorExpression(names, reset);
            else
                nex = new NandExpression(names, reset);
            break;
        case Node.CELEMENT:
            switch (root.size()) {
            case 2:
                nex = new CElementExpression(names[0], names[1], reset);
                break;
            case 3:
                nex = new And2C2Expression(names[0], names[1],
                                           names[2], reset);
                break;
            case 4:
                nex = new TwoAnd2C2Expression(names[0], names[1],
                                              names[2], names[3], reset);
                break;
            }
            break;
        }

        if (nex != null) {
            Section sect = ((Doodad)root.getCookie()).section;
            Direction dir = (root.getSense() == Node.ACTIVE_LOW ?
                             Direction.DOWN : Direction.UP);
            WholeOperator wop = new WholeOperator(dest, nex, dir, sect);
            wos.hereYaGo(wop);
            decl.declareNode(root);
        }

        for (int i = 0; i < root.size(); i++)
            emitTree(root.get(i), wos, decl, false);
    }

    /**
     * Does almost everything you want with a completion tree.
     * @param  ops  an array of arrays of nodes, where the outer array
     *              represents the c-element tree, and the inner arrays
     *              represent ors.  Each inner array must contain nodes
     *              with the same level, but different inner arrays can have
     *              nodes at different levels.
     * @param  desiredSense  desired sense of root node-- may be
     *                       ACTIVE_HIGH or ACTIVE_LOW to be specific,
     *                       or UNKNOWN_SENSE to accept either.
     * @param  wos    the final completion tree is output here
     * @param  decl   intermediate nodes are declared here
     * @param  reset  true means add _Reset to the root node
     * @param  rootName the name to give the root node
     * @return root node of the completion tree
     */
    public static Node doCompletion(Node[][] ops, int desiredSense,
                                    WholeOperatorSink wos, Declarations decl,
                                    boolean reset, String rootName,
                                    int[] rootIndex) {
        // a little bit of error checking
        if (ops.length == 0)
            throw new IllegalArgumentException("ops.length == 0");
        for (int i = 0; i < ops.length; i++)
            if (ops[i].length == 0)
                throw new IllegalArgumentException("ops[" + i
                                                   + "].length == 0");

        Node root = assignNames(shiftInvertersUp(TreeReadjuster.readjust(GreedyComplete.complete(ops, desiredSense))), rootName, rootIndex);
        emitTree(root, wos, decl, reset);
        return root;
    }

    /**
     * Same as above, but does multiple, separate completion trees at
     * once.  (The point of this is to share the generated node-name
     * space across all completion trees.)
     */
    public static void doCompletion(WholeOperatorSink wos, Declarations decl,
                                    CompletionInfo[] info) {
        Node[] unnamedRoots = new Node[info.length];
        String[] rootNames = new String[info.length];
        for (int j = 0; j < info.length; j++) {
            Node[][] ops = info[j].ops;
            int desiredSense = info[j].desiredSense;

            // a little bit of error checking
            if (ops.length == 0)
                throw new IllegalArgumentException("ops.length == 0");
            for (int i = 0; i < ops.length; i++)
                if (ops[i].length == 0)
                    throw new IllegalArgumentException("ops[" + i
                                                       + "].length == 0");

            unnamedRoots[j] =
                TreeReadjuster.readjust(GreedyComplete.complete(ops,
                                                                desiredSense));
            unnamedRoots[j] = shiftInvertersUp(unnamedRoots[j]);
            rootNames[j] = info[j].rootName;
        }

        Node[] roots = assignNames(unnamedRoots, rootNames);
        assert (roots.length == info.length);

        for (int j = 0; j < info.length; j++) {
            emitTree(roots[j], wos, decl, info[j].reset);
            info[j].root = roots[j];
        }
    }
}
