/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.complete;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/*
  What's the point of this file:

  GreedyComplete constructs a completion tree which is as greedy as
  possible, i. e. it may make excessive use of and2c2s, and end up
  with extra inverters at the end, much like the original PReSto
  algorithm did.  The idea is to use GreedyComplete to create an
  initial completion tree, and then use TreeReadjuster to make the
  tree less greedy.  (So, the resulting tree will have the same number
  of levels as the greedy tree, but it will try to use fewer aggressive
  operators.)

  To use:

  Just call the complete() method, which is the only public interface.
 */

public class GreedyComplete {
    /**
     * Takes a List of Node[] arrays which each have one or
     * two elements.  Returns a List with the same Node[] arrays
     * in a different order, according to the following two rules:
     *   1) arrays with one element and arrays with two elements will
     *      be interleaved as much as possible
     *   2) the last array in the List will be an array with two
     *      elements, if any exist
     * The first constraint causes the completion generator to
     * prefer and2c2 over 2and2c2.  The second constraint ensures
     * that if there's an odd number of Node[] arrays, the one
     * that doesn't get into a C-element will be a nand2, rather
     * than just an inverter, if there are any two-element arrays.
     * (This reduces the number of and2c2s by one.)
     */
    private static List alternateOnesAndTwos(List orig) {
        /* First, build up two lists, one with one-element arrays
         * and one with two-element arrays.  Note that we build
         * the lists in reverse order, which will be important
         * in the next step. */
        LinkedList ones = new LinkedList();
        LinkedList twos = new LinkedList();

        for (Iterator it = orig.iterator(); it.hasNext(); ) {
            Node[] a = (Node[]) it.next();
            switch (a.length) {
            case 1:
                ones.addFirst(a);
                break;
            case 2:
                twos.addFirst(a);
                break;
            default:
                assert false;
            }
        }

        /* Next, build up the result list, alternating as much as
         * possible.  Note that we again build it in reverse
         * order, which undoes the reversal we did in the first
         * step.  This double-reversal is done for more than
         * the sake of perversity-- it is how we satisfy
         * constraint #2, by starting with a 2-element array.
         */
        LinkedList result = new LinkedList();
        Iterator it1, it2;

        for (it1 = ones.iterator(), it2 = twos.iterator();
             it1.hasNext() || it2.hasNext(); ) {
            if (it2.hasNext())
                result.addFirst(it2.next());
            if (it1.hasNext())
                result.addFirst(it1.next());
        }

        return result;
    }

    /**
     * Creates a Node which C-elements together x and y.
     * x and y must be one or two element arrays, and
     * it chooses c2, and2c2, or 2and2c2 appropriately.
     */
    private static Node makeCelement(Node[] x, Node[] y) {
        if (y.length == 2 && x.length == 1) {
            // flip around to satisfy Node's requirement for and2c2 order
            Node[] tmp = x;
            x = y;
            y = tmp;
        }

        Node[] children = new Node[x.length + y.length];
        System.arraycopy(x, 0, children, 0, x.length);
        System.arraycopy(y, 0, children, x.length, y.length);

        return new Node(Node.CELEMENT, null, null, x[0].getTier() + 1,
                        null, children);
    }

    /**
     * Given the number of rails to be ored together, and the maximum
     * number of rails which can be ored together in one group,
     * partitions the rails into groups which are as balanced as
     * possible.
     * @param   maxterms   maximum size of a group
     * @param   numRails   total number of rails to partition
     * @return  an array with one element for each group, containing
     *          the number of rails which should be in that group.
     */
    private static int[] calculateBalancedGroups(int maxterms, int numRails) {
        /* Implementation note: this uses a very convoluted algorithm
         * to compute a very simple result, but it makes sense to me.
         * If it bothers you, replace it with a simpler algorithm. */

        int q = numRails / maxterms;
        int r = numRails % maxterms;
        int[] result = new int[q + Math.min(1, r)];
        Arrays.fill(result, maxterms);
        if (r != 0)
            result[q] = r;

        if (result.length > 1) {
            boolean changed;

            do {
                changed = false;
                int i;
                int max = -1;
                for (i = 0; i < result.length - 1; i++) {
                    if (max < 0 || result[i] > result[max])
                        max = i;
                }

                if (result[i] < (result[max] - 1)) {
                    changed = true;
                    result[max]--;
                    result[i]++;
                }

            } while (changed);
        }

        Arrays.sort(result);
        return result;
    }

    /**
     * Given an array of nodes, add a new tier of nodes, oring
     * together the original nodes as much as possible.  Returns
     * the new, smaller array of nodes after the tier has been added.
     */
    private static Node[] doOrs(Node[] rails) {
        int tier = rails[0].getTier() + 1;
        boolean nand = ((tier & 1) == 0);
        int maxterms = (nand ? 5 : 3);
        int[] groups = calculateBalancedGroups(maxterms, rails.length);
        
        Node[] result = new Node[groups.length];
        int j = 0;

        /* Group the rails together */
        for (int k = 0; k < rails.length; j++) {
            int size = groups[j];
            Node[] children = new Node[size];

            for (int i = 0; i < size; i++, k++)
                children[i] = rails[k];

            result[j] = new Node(Node.NOR, null, null, tier, null, children);
        }

        return result;
    }

    /**
     * Recursively computes each tier of the tree, by adding one
     * tier of nodes to the tree, and then calling itself with the
     * new, smaller ops array, until there is only one node,
     * which it then returns.
     */
    private static Node recurse(Node[][] ops, int tier) {
        assert ops.length > 0 : "no ops!";
        if (ops.length == 1 && ops[0].length == 1)
            return ops[0][0];

        boolean and2c2ok = ((tier & 1) == 0);

        List newChannels = new ArrayList();
        List wantToCombine = new ArrayList();

        for (int i = 0; i < ops.length; i++) {
            Node[] rails = ops[i];
            int nrails = rails.length;

            int maxCombinable = (and2c2ok ? 2 : 1);

            if (rails[0].getTier() != tier - 1) {
                assert rails[0].getTier() > tier - 1;
                newChannels.add(rails);
            } else if (nrails > maxCombinable) {
                newChannels.add(doOrs(rails));
            } else {
                wantToCombine.add(rails);
            }
        }

        assert wantToCombine.size() > 0 || newChannels.size() > 0:
            "wantToCombine.size() = " + wantToCombine.size() + ", " +
            "newChannels.size() = " + newChannels.size();

        if (and2c2ok)
            wantToCombine = alternateOnesAndTwos(wantToCombine);

        assert wantToCombine.size() > 0 || newChannels.size() > 0;

        for (Iterator it = wantToCombine.iterator(); it.hasNext(); ) {
            Node[] rails1 = (Node[]) it.next();
            if (!it.hasNext()) {
                // an odd number, so this one can't go through
                // a C-element
                newChannels.add(doOrs(rails1));
                break;
            }

            Node[] rails2 = (Node[]) it.next();
            newChannels.add(new Node[] { makeCelement(rails1, rails2) });
        }

        assert newChannels.size() > 0;

        return recurse((Node[][])
                       newChannels.toArray(ZERO_LENGTH_NODE_ARRAY_ARRAY),
                       tier + 1);
    }

    /**
     * Returns a tree of Nodes which make up the desired completion tree.
     * @param  ops  an array of arrays of nodes, where the outer array
     *              represents the c-element tree, and the inner arrays
     *              represent ors.  Each inner array must contain nodes
     *              with the same level, but different inner arrays can have
     *              nodes at different levels.
     * @param  desiredSense  desired sense of root node-- may be
     *                       ACTIVE_HIGH or ACTIVE_LOW to be specific,
     *                       or UNKNOWN_SENSE to accept either.
     */
    public static Node complete(Node[][] ops, int desiredSense) {
        Node result = recurse(ops, 0);
        // get desired sense
        if (desiredSense != Node.UNKNOWN_SENSE &&
            desiredSense != result.getSense())
            result = doOrs(new Node[] { result })[0];
        return result;
    }

    private static final Node[][] ZERO_LENGTH_NODE_ARRAY_ARRAY = new Node[0][];
}
