/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.complete;

import java.util.ArrayList;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class TreeReadjuster {
    /**
     * Takes the given node, and splits it into two nor nodes,
     * and adds these two nodes to dest.  If n has an odd number
     * of children, then the first new node has one more child
     * than the second new node.  (This property is important
     * when we split and2c2s!)
     */
    private static void splitNode(Node n, Collection dest) {
        Node[] b = new Node[n.size() / 2];
        Node[] a = new Node[n.size() - b.length];
        for (int i = 0; i < n.size(); i++)
            if (i < a.length)
                a[i] = n.get(i);
            else
                b[i - a.length] = n.get(i);
        dest.add(new Node(Node.NOR, null, null, n.getTier(), null, a));
        dest.add(new Node(Node.NOR, null, null, n.getTier(), null, b));
        assert a.length >= b.length;
    }

    /**
     * Takes a completion tree and attempts to reduce its cost
     * by sucking operators closer to the root.  (I don't formally
     * define a cost function, but the basic idea is to try to
     * get rid of and2c2s, 2and2c2s, and nor3s by moving portions of
     * them "up" into smaller nands or plain inverters.)
     */
    public static Node readjust(Node root) {
        int type = root.getType();
        int sense = root.getSense();

        if (type == Node.TERMINAL)
            return root;

        int newType;
        List newChildren = new ArrayList();

        if (type == Node.NOR && root.size() == 1) {
            /* It's an inverter.  Split the child node in half, if possible. */
            Node child = root.get(0);
            if (child.getType() == Node.TERMINAL)
                return root;    // nothing more we can do
            if (child.size() == 1) // back-to-back inverters
                return readjust(child.get(0)); // delete both inverters
            newType = child.getType();
            splitNode(child, newChildren);
        } else if (type == Node.NOR && sense == Node.ACTIVE_HIGH) {
            /* It's a nand.  See if there are nor3s below which could be
             * reduced to nor2s, without making the nand too big. */
            List nor3s = new ArrayList();
            for (int i = 0; i < root.size(); i++) {
                Node child = root.get(i);
                if (child.getType() == Node.NOR && child.size() == 3)
                    nor3s.add(child);
            }
            int numNewNor2s = (nor3s.size() / 2) + (nor3s.size() % 2);
            Map repl = new IdentityHashMap();
            Map ins = new IdentityHashMap();
            if (numNewNor2s > 0 && numNewNor2s + root.size() < 6) {
                /* We're going to convert the nor3s to nor2s, and add
                 * some new nor2s to the nand. */
                for (Iterator it = nor3s.iterator(); it.hasNext(); ) {
                    List x = new ArrayList();
                    Node n = (Node) it.next();
                    x.add(n.get(2));
                    repl.put(n, new Node(Node.NOR, null, null, n.getTier(),
                                         null,
                                         new Node[] { n.get(0), n.get(1) }));
                    if (it.hasNext()) {
                        Node m = (Node) it.next();
                        x.add(m.get(0));
                        repl.put(m, new Node(Node.NOR, null, null, m.getTier(),
                                             null,
                                             new Node[] { m.get(1),
                                                          m.get(2) }));
                    }
                    ins.put(n, new Node(Node.NOR, null, null, n.getTier(),
                                        null, x));
                }
            }
            /* copy children, replacing nodes listed in repl and inserting
             * nodes listed in ins. */
            newType = Node.NOR;
            for (int i = 0; i < root.size(); i++) {
                Node child = root.get(i);
                Node replacement = (Node) repl.get(child);
                newChildren.add((replacement == null) ? child : replacement);
                Node insertion = (Node) ins.get(child);
                if (insertion != null)
                    newChildren.add(insertion);
            }
        } else {
            // Don't change it; just copy it
            newType = type;
            for (int i = 0; i < root.size(); i++) {
                Node child = root.get(i);
                newChildren.add(child);
            }
        }

        List processedChildren = new ArrayList();
        for (Iterator it = newChildren.iterator(); it.hasNext(); )
            processedChildren.add(readjust((Node)it.next()));

        return new Node(newType, null, null, root.getTier(), null,
                        processedChildren);
    }
}
