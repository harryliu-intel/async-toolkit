/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.avlsi.file.common.HierName;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.graph.Edge;
import com.avlsi.util.graph.Vertex;

class HalfOp implements Vertex {
    private final Node node;
    private final int dir;
    private final HalfOpGraph graph;
    private double slew;
    private final boolean isDuplicate;
    private final Map<HalfOp, HalfOpEdge> outEdges;
    private final Map<HalfOp, HalfOpEdge> deletedEdges;
    private final Map<HalfOp, HalfOpEdge> insertedEdges;

    public HalfOp(HalfOpGraph graph, Node node, int dir) {
        this.graph = graph;
        this.node = node;
        this.dir = (dir == 0) ? 0 : 1;
        this.slew = 0.0;
        this.isDuplicate=false;

        this.outEdges = new HashMap<HalfOp, HalfOpEdge>();
        this.deletedEdges = new HashMap<HalfOp, HalfOpEdge>();
        this.insertedEdges = new HashMap<HalfOp, HalfOpEdge>();
    }

    HalfOp(HalfOp o) {
        this.graph = o.graph;
        this.node = o.node;
        this.dir = o.dir;
        this.slew = 0.0;
        this.isDuplicate = true;
        this.outEdges = copyEdges(o);
        this.deletedEdges = new HashMap<HalfOp, HalfOpEdge>();
        this.insertedEdges = new HashMap<HalfOp, HalfOpEdge>();
    }

    private Map<HalfOp, HalfOpEdge> copyEdges(HalfOp o) {
        Map<HalfOp, HalfOpEdge> result = new HashMap<HalfOp, HalfOpEdge>();
        for(HalfOpEdge e: o.outEdges.values()) {
            result.put(e.getSnk(), 
                       graph.newHalfOpEdge(this, e.getSnk(), e.getRule()));
        }
        return result;
    }

    @Override public HalfOp duplicate() {
        assert(!isDuplicate);
        return graph.getDuplicatedHalfOp(node, dir);
    }

    @Override public void deleteEdge(Vertex v) {
        HalfOp h = (HalfOp) v;
        HalfOpEdge e = outEdges.get(h);
        if (e != null) {
            deletedEdges.put(h,e);
        }
    }

    /* TODO(piyush): addEdge currently assumes that edges are only
     * added to duplicatedHalfOp.
     */
    @Override public void addEdge(Vertex v) {
        HalfOp h = (HalfOp) v;
        assert(h.isDuplicate);
        HalfOp o = graph.getHalfOp(h.getNode(), h.getDir());
        HalfOpEdge oe = outEdges.get(o);
        HalfOpEdge e = graph.newHalfOpEdge(this, h, oe.getRule());
        if (e != null) {
            deletedEdges.put(h,e);
        }
    }

    @Override public void restore() {
        deletedEdges.clear();
        insertedEdges.clear();
        graph.deleteDuplicatedHalfOp(node, dir);
    }

    boolean isDuplicate() {
        return isDuplicate;
    }

    void getEdgesFromNode() {
        final HalfOp src = this;
        getNode().foreachRule(
            new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (r.asserted || sense != getDir()) {
                        return;
                    }
                    final HalfOp snk = graph.getHalfOp(r.target(), r.dir);
                    if (!outEdges.containsKey(snk)) {
                        final HalfOpEdge e = graph.newHalfOpEdge(src, snk, r);
                        outEdges.put(snk, e);
                    }
                }
            });
    }

    public Node getNode() {
        return node;
    }

    public int getDir() {
        return dir;
    }

    public double getSlew() {
        return this.slew;
    }

    public void setSlew(double slew) {
        this.slew = slew;
        node.setSlew((float) slew);
    }

    @Override public String toString() {
        return getHalfOpName(getNode(), getDir());
    }
    
    private String uniqId() {
        return (isDuplicate ? "#T#" : "#F#") + toString();
    }

    static String getHalfOpName(Node n, int dir) {
        return getHalfOpName(n.getName(), dir);
    }

    static String getHalfOpName(HierName n, int dir) {
        return n.getAspiceString() + ((dir == 0) ? "-" : "+");
    }

    @Override public int compareTo(Vertex v) {
        if (v == null) {
            throw new NullPointerException();
        }
        HalfOp h = (HalfOp) v;
        return this.uniqId().compareTo(h.uniqId());
    }
    
    @Override public boolean equals(Object o) {
        return (o != null) && (o instanceof HalfOp) && 
            this.compareTo((HalfOp) o) == 0;
    }

    @Override public int hashCode() {
        return uniqId().hashCode();
    }

    public HalfOp not() {
        return graph.getHalfOp(node, 1-getDir());
    }

    public Collection<HalfOp> enables() {
        Set<HalfOp> ret = outEdges.keySet();
        ret.removeAll(deletedEdges.keySet());
        ret.addAll(insertedEdges.keySet());
        return ret;
    }

    public Collection<HalfOp> disables() {
        return this.not().enables();
    }

    public Pair<String,String> getLocalName() {
        return graph.localize(this);
    }

    @Override public void foreachEdge(final UnaryPredicate<Edge> func) {
        foreachEdge(func, outEdges, deletedEdges);
        foreachEdge(func, insertedEdges, deletedEdges);
    }

    private void foreachEdge(final UnaryPredicate<Edge> func, 
                             Map<HalfOp, HalfOpEdge> edges,
                             Map<HalfOp, HalfOpEdge> exclEdges) {
        for (Map.Entry<HalfOp, HalfOpEdge> e: edges.entrySet()) {
            if(!exclEdges.containsKey(e.getKey())) {
                func.evaluate(e.getValue());
            }
        }
    }

    HalfOpEdge getEdge(HalfOp v) {
        return outEdges.get(v);
    }

    int numEdges() {
        return this.outEdges.keySet().size();
    }
}
