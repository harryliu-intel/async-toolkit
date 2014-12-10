/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

import java.util.LinkedList;
import java.util.zip.CRC32;

/**
 * This class is for representing an arbitrary collection of
 * pull-ups and pull-downs, which cannot be expressed with
 * a short-hand operator.
 */

public class ArbitraryNodeExpression implements NodeExpression {
    private final NodeName[][] ups;
    private final NodeName[][] downs;
    private CRC32 hc = null;

    public ArbitraryNodeExpression(NodeName[][] ups, NodeName[][] downs) {
        this.ups = ups;
        this.downs = downs;
    }

    private static class Line extends AbstractPrsLine {
        private final Direction dir;
        private final NodeName[] nodes;
        private final NodeName dest;
        private final Direction preferredDirection;

        Line(Direction dir, NodeName[] nodes, NodeName dest,
             Direction preferredDirection) {
            this.dir = dir;
            this.nodes = nodes;
            this.dest = dest;
            this.preferredDirection = preferredDirection;
        }

        public boolean isParameterized() {
            for (int i = 0; i < nodes.length; i++)
                if (nodes[i].isParameterized())
                    return true;
            return dest.isParameterized();
        }

        public String toStringWithIndex(int index) {
            StringBuffer result = new StringBuffer();
            for (int i = 0; i < nodes.length; i++) {
                if (i != 0)
                    result.append(" & ");
                if (dir == Direction.UP)
                    result.append('~');
                result.append(nodes[i].toStringWithIndex(index));
            }

            result.append(" -> ");
            result.append(dest.toStringWithIndex(index));
            result.append(dir.toString());
            return result.toString();
        }

        public String toStringWithVariable(String var) {
            StringBuffer result = new StringBuffer();
            for (int i = 0; i < nodes.length; i++) {
                if (i != 0)
                    result.append(" & ");
                if (dir == Direction.UP)
                    result.append('~');
                result.append(nodes[i].toStringWithVariable(var));
            }

            result.append(" -> ");
            result.append(dest.toStringWithVariable(var));
            result.append(dir.toString());
            return result.toString();
        }
        
        public boolean isIndented() {
            return dir != preferredDirection;
        }
    }

    private static final LineAndSection[] LAS0 = new LineAndSection[0];

    public LineAndSection[] evaluate(NodeName dest,
                                     Direction preferredDirection,
                                     Section upSection, Section downSection) {
        LinkedList l = new LinkedList();
        for (int i = 0; i < ups.length; i++) {
            l.add(new LineAndSection(new Line(Direction.UP, ups[i], dest,
                                              preferredDirection), upSection));
        }

        for (int i = 0; i < downs.length; i++) {
            l.add(new LineAndSection(new Line(Direction.DOWN, downs[i], dest,
                                              preferredDirection), downSection));
        }

        return (LineAndSection[]) l.toArray(LAS0);
    }

    private NodeName[][] unparam(NodeName[][] old, int index) {
        LinkedList l = new LinkedList();
        for (int i = 0; i < old.length; i++) {
            NodeName[] a = old[i];
            NodeName[] b = new NodeName[a.length];
            for (int j = 0; j < a.length; j++)
                b[j] = a[j].unparameterize(index);
            l.add(b);
        }

        return (NodeName[][]) l.toArray(new NodeName[0][]);
    }

    private NodeName[][] canon(NodeName[][] old, NodeCanonicalizer c) {
        LinkedList l = new LinkedList();
        for (int i = 0; i < old.length; i++) {
            NodeName[] a = old[i];
            NodeName[] b = new NodeName[a.length];
            for (int j = 0; j < a.length; j++)
                b[j] = c.canonicalize(a[j]);
            l.add(b);
        }

        return (NodeName[][]) l.toArray(new NodeName[0][]);
    }

    public NodeExpression unparameterize(int index) {
        return new ArbitraryNodeExpression(unparam(ups, index),
                                           unparam(downs, index));
    }

    public NodeExpression canonicalize(NodeCanonicalizer c) {
        return new ArbitraryNodeExpression(canon(ups, c),
                                           canon(downs, c));
    }

    private class ANSimplification implements Simplification {
        boolean isPower(Direction dir, int i, int j, Power p) {
            NodeName[] row;

            if (dir == Direction.UP)
                row = ups[i];
            else
                row = downs[i];

            return (row[j] == p);
        }

        public boolean isSimpler() {
            /* Check if it can be simplified (to an alias) because
             * there's only a pull up or only a pull down */
            if (ups.length == 0 || downs.length == 0)
                return true;

            // Check if it can be simplified because of Vdd or GND
            for (int i = 0; i < ups.length; i++) {
                NodeName[] row = ups[i];
                for (int j = 0; j < row.length; j++)
                    if (isPower(Direction.UP, i, j, Power.Vdd) ||
                        isPower(Direction.UP, i, j, Power.GND))
                        return true;
            }

            for (int i = 0; i < downs.length; i++) {
                NodeName[] row = downs[i];
                for (int j = 0; j < row.length; j++)
                    if (isPower(Direction.DOWN, i, j, Power.Vdd) ||
                        isPower(Direction.DOWN, i, j, Power.GND))
                        return true;
            }

            return false;
        }
    }

    public Simplification getSimplification() {
        return new ANSimplification();
    }

    private static class AlwaysTrue extends Exception {
        public final Direction dir;

        AlwaysTrue(Direction dir) {
            this.dir = dir;
        }
    }

    /**
     * Returns a copy of x, with any node flagged as matching deleteMe
     * by simp (with direction dir) deleted, and any line containing a
     * node flagged as matching deleteLine by simp (with direction dir)
     * deleted.
     *
     * Throws AlwaysTrue (with dir) if it deletes all the nodes from a
     * line (and therefore the expression would always be true).
     *
     * Throws AlwaysTrue (with dir.opposite()) if it deletes all the lines
     * (or there weren't any lines to begin with).
     */
    private NodeName[][] deleteStuff(NodeName[][] x,
                                     Direction dir, ANSimplification simp,
                                     Power deleteMe, Power deleteLine)
        throws AlwaysTrue {
        LinkedList result = new LinkedList();

        outer:
        for (int i = 0; i < x.length; i++) {
            NodeName[] a = x[i];
            LinkedList line = new LinkedList();

            for (int j = 0; j < a.length; j++) {
                if (simp.isPower(dir, i, j, deleteLine))
                    continue outer;
                if (!simp.isPower(dir, i, j, deleteMe))
                    line.add(a[j]);
            }

            if (line.size() == 0)
                throw new AlwaysTrue(dir);

            result.add(line.toArray(new NodeName[0]));
        }

        if (result.size() == 0)
            throw new AlwaysTrue(dir.opposite());

        return (NodeName[][]) result.toArray(new NodeName[0][]);
    }

    public NodeExpression applySimplification(Simplification simp) {
        if (!simp.isSimpler())
            return this;

        try {
            ANSimplification ans = (ANSimplification) simp;
            return new ArbitraryNodeExpression(deleteStuff(ups, Direction.UP,
                                                           ans, Power.GND,
                                                           Power.Vdd),
                                               deleteStuff(downs,
                                                           Direction.DOWN,
                                                           ans, Power.Vdd,
                                                           Power.GND));
        } catch (AlwaysTrue e) {
            return new AliasNodeExpression(((e.dir == Direction.UP) ?
                                            Power.Vdd : Power.GND), 0);
        }
    }

    public int hashCode() {
        if (hc == null) {
            hc = new CRC32();

            for (int i = 0; i < ups.length; i++) {
                NodeName[] a = ups[i];
                for (int j = 0; j < a.length; j++)
                    hc.update(a[j].hashCode() % 251);
            }
            
            for (int i = 0; i < downs.length; i++) {
                NodeName[] a = downs[i];
                for (int j = 0; j < a.length; j++)
                    hc.update(a[j].hashCode() % 241);
            }
        }

        return (int) hc.getValue();
    }

    public boolean equals(Object o) {
        if (o instanceof ArbitraryNodeExpression) {
            ArbitraryNodeExpression x = (ArbitraryNodeExpression) o;

            if (ups.length != x.ups.length || downs.length != x.downs.length)
                return false;

            for (int i = 0; i < ups.length; i++) {
                NodeName[] a = ups[i];
                NodeName[] b = x.ups[i];
                if (a.length != b.length)
                    return false;
                for (int j = 0; j < a.length; j++)
                    if (!a[j].equals(b[j]))
                        return false;
            }
            
            for (int i = 0; i < downs.length; i++) {
                NodeName[] a = downs[i];
                NodeName[] b = x.downs[i];
                if (a.length != b.length)
                    return false;
                for (int j = 0; j < a.length; j++)
                    if (!a[j].equals(b[j]))
                        return false;
            }
            
            return true;
        }

        return false;
    }
}
