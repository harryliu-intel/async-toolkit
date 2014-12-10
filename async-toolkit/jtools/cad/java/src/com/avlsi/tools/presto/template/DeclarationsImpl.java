/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import com.avlsi.tools.presto.ChannelName;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import com.avlsi.util.container.FlatteningIterator;
import com.avlsi.util.container.IntArrayComparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import com.avlsi.tools.presto.complete.Node;
import com.avlsi.util.container.Pair;
import java.io.PrintWriter;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class DeclarationsImpl implements Declarations {
    /**
     * maps Pair(String type, String name) -> Set(int[])
     */
    private final TreeMap decls = new TreeMap();

    private void declare(String type, String name, int[] indices) {
        Pair key = new Pair(type, name);
        Set value = (Set) decls.get(key);
        if (value == null) {
            value = new TreeSet(new IntArrayComparator());
            decls.put(key, value);
        }
        value.add(indices);
    }

    public void declareChannel(ChannelName c, String type) {
        declare(type + c.get1of(), c.getName(), c.getArrayIndices());
    }

    public void declareNode(Node n) {
        declare("node", n.getName(), n.getArrayIndices());
    }

    private static class Range {
        final int from;
        int to;

        Range(int x) {
            from = x;
            to = x;
        }

        public String toString() {
            if (from == to)
                return Integer.toString(from);
            else
                return from + ".." + to;
        }
    }

    /**
     * For a given index, tries to coalesce arrays of ranges which are
     * equal in all other indices and consecutive in the given index.
     * @param ranges   a List of arrays of Ranges
     * @param i        index into array of Ranges
     */
    private static void coalesceOnce(List ranges, final int i) {
        /* Sort the given list of arrays of ranges, using index i
         * as the least significant position. */
        Collections.sort(ranges, new Comparator() {
                public int compare(Object o1, Object o2) {
                    Range[] a = (Range[]) o1;
                    Range[] b = (Range[]) o2;
                    assert (a.length == b.length);
                    assert (i >= 0 && i < a.length);
                    
                    for (int j = 0; j < a.length; j++) {
                        int x = (i + j + 1) % a.length;
                        int d = (a[x].from - b[x].from);
                        if (d != 0)
                            return d;
                        d = (a[x].to - b[x].to);
                        if (d != 0)
                            return d;
                    }

                    return 0;
                }
            });

        Range[] prev = null;

        // now we just need to coalesce adjacent arrays if legal
        for (Iterator it = ranges.iterator(); it.hasNext(); ) {
            Range[] a = (Range[]) it.next();
            
            boolean ok = (prev != null);
            for (int j = 0; ok && j < a.length; j++)
                if (j != i)
                    ok = (a[j].from == prev[j].from &&
                          a[j].to == prev[j].to);

            if (ok && a[i].from == prev[i].to + 1) {
                assert (prev[i].to >= prev[i].from);
                assert (a[i].to == a[i].from);
                prev[i].to++;
                it.remove();
            } else {
                prev = a;
            }
        }
    }

    /**
     * Given a collection of int[], tries to coalesce sparse
     * array declarations into one or more nice array ranges
     * (i. e. "[0..3,0..1]") and returns these ranges as an
     * array of strings.
     */
    private static String[] coalesce(Collection indices) {
        assert (indices.size() > 0);

        int len = ((int[])indices.iterator().next()).length;
        if (len == 0)
            return new String[] { "" };

        LinkedList ranges = new LinkedList();

        for (Iterator it = indices.iterator(); it.hasNext(); ) {
            int[] a = (int[]) it.next();
            assert (a.length == len);
            Range[] r = new Range[len];
            for (int i = 0; i < len; i++)
                r[i] = new Range(a[i]);
            ranges.add(r);
        }

        for (int i = 0; i < len; i++)
            coalesceOnce(ranges, i);

        LinkedList result = new LinkedList();
        for (Iterator it = ranges.iterator(); it.hasNext(); ) {
            Range[] r = (Range[]) it.next();
            StringBuffer buf = new StringBuffer();
            char c = '[';
            for (int i = 0; i < len; i++) {
                buf.append(c);
                buf.append(r[i]);
                c = ',';
            }
            buf.append(']');
            result.add(buf.toString());
        }

        return (String[]) result.toArray(new String[0]);
    }

    /**
     * Prints the declarations to the given PrintWriter.
     * @param  w      print the declarations here
     * @param  indent number of spaces to indent each line
     * @param  max    longest line length allowed
     */
    public void emit(PrintWriter w, int indent, int max) {
        // twiddle with the order, because I want the nodes to be first
        Pair where = new Pair("node", "");
        List reordered = new LinkedList();
        reordered.add(decls.tailMap(where).entrySet().iterator());
        reordered.add(decls.headMap(where).entrySet().iterator());

        String currentType = null;
        StringBuffer buf = new StringBuffer();

        for (Iterator it = new FlatteningIterator(reordered.iterator());
             it.hasNext(); ) {
            Map.Entry e = (Map.Entry) it.next();
            Pair key = (Pair) e.getKey();
            Set value = (Set) e.getValue();
            String type = (String) key.getFirst();
            String name = (String) key.getSecond();
            String[] indices = coalesce(value);
            for (int i = 0; i < indices.length; i++) {
                String x = name + indices[i];
                if (currentType == null || !currentType.equals(type) ||
                    buf.length() + x.length() + 3 > max) {
                    buf.append(';');
                    if (currentType != null)
                        w.println(buf.toString());
                    buf.setLength(indent);
                    for (int j = 0; j < indent; j++)
                        buf.setCharAt(j, ' ');
                    buf.append(type);
                    currentType = type;
                } else {
                    buf.append(',');
                }
                buf.append(' ');
                buf.append(x);
            }
        }

        buf.append(';');
        w.println(buf.toString());
    }
}
