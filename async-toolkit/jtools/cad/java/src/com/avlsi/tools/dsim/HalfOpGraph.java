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
import java.util.LinkedList;
import java.util.List;

import com.avlsi.file.common.HierName;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.graph.Edge;
import com.avlsi.util.graph.Graph;
import com.avlsi.util.graph.Vertex;


class HalfOpGraph implements Graph {
    private DSim dsim;
    private HashMap<String, HalfOp> halfOps;
    private HashMap<String, HalfOp> duplicatedHalfOps;

    public HalfOpGraph(DSim dsim) {
        this.dsim = dsim;
        halfOps = new HashMap<String, HalfOp>();
        duplicatedHalfOps = new HashMap<String, HalfOp>();
    }

    Collection<Edge> getExcludePaths(HalfOp h) {
        Collection<List<Pair<Node,Integer>>> paths =
            dsim.getSlintIgnoreFrom(h.getNode(), h.getDir());
        /*
         * TODO(piyush): Currently only supports 1 exclusion path per node.
         */
        assert(paths.size() <= 1);

        List<Edge> excludePath = new LinkedList<Edge>();
        HalfOp prev = null;
        excludePath.clear();
        for (List<Pair<Node,Integer>> path : paths) {
            for (Pair<Node, Integer> hop : path) {
                HalfOp curr = getHalfOp(hop.getFirst(), hop.getSecond());
                if(prev != null) {
                    excludePath.add(prev.getEdge(curr));
                }
                prev = curr;
            }
        }
        return excludePath;
    }

    @Override public void foreachVertex(final UnaryPredicate<Vertex> func) {
        dsim.foreachNode(
            new UnaryPredicate<Node>(){
                public boolean evaluate(Node n) {
                    if (n.getName().isVdd() || 
                        n.getName().isGND() ||
                        n.getName().isResetNode() ) {
                        return true;
                    }      
                    for(int i = 0; i < 2; ++i) {
                        HalfOp h = getHalfOp(n, i);
                        func.evaluate(h);
                    }
                    return true;
                }
            });
    }

    public HalfOp getHalfOp(Node n, int dir) {
        String halfOpName = HalfOp.getHalfOpName(n,dir);
        HalfOp h = halfOps.get(halfOpName);
        if(h == null) {
            h = new HalfOp(this, n, dir);
            halfOps.put(halfOpName, h);
        }
        return h;
    }

    HalfOp getHalfOp(String name) {
        return halfOps.get(name);
    }

    public HalfOpEdge newHalfOpEdge(HalfOp src, HalfOp snk, Rule r) {
        return new HalfOpEdge(src, snk, r);
    }

    HalfOp getDuplicatedHalfOp(Node n, int dir) {
        String halfOpName = HalfOp.getHalfOpName(n,dir);
        HalfOp h = duplicatedHalfOps.get(halfOpName);
        if(h == null) {
            HalfOp original = halfOps.get(halfOpName);
            if (original != null) {
                h = new HalfOp(original);
                duplicatedHalfOps.put(halfOpName, h);
            }
        }
        return h;
    }

    void deleteDuplicatedHalfOp(Node n, int dir) {
        String halfOpName = HalfOp.getHalfOpName(n,dir);
        duplicatedHalfOps.remove(halfOpName);
    }

    public Pair<String, String> localize(HalfOp h) {
        Triplet<HierName, CadenceInfo, HierName> localInfo = 
            dsim.localize(h.getNode().getName());
        return new Pair<String, String>(
             localInfo.getSecond().getType(),
             HalfOp.getHalfOpName(localInfo.getThird(), h.getDir()));
    }

    void init() {
        dsim.foreachNode(
            new UnaryPredicate<Node>(){
                public boolean evaluate(Node n) {
                    if (n.getName().isVdd() || 
                        n.getName().isGND() ||
                        n.getName().isResetNode() ) {
                        return true;
                    }      
                    for(int i = 0; i < 2; ++i) {
                        HalfOp h = getHalfOp(n, i);
                        h.getEdgesFromNode();
                    }
                    return true;
                }
            });
    }

    boolean isDynamic(HalfOp h) {
        return dsim.isDynamic(h.getNode());
    }

    boolean hasFullStaticizer(HalfOp h) {
        return dsim.hasFullStaticizer(h.getNode());
    }

    HalfOp getStaticizerInverse(HalfOp h) {
        Node si = dsim.getStaticizerInverse(h.getNode());
        if(si != null) {
            return getHalfOp(si, 1-h.getDir());
        }
        return null;
    }
}
