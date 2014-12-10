/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class ShortestPaths {
    private Map<Vertex, Double> distances;
    private Map<Vertex, Vertex> parents;
    private final Vertex src;

    public ShortestPaths(Vertex src) {
        distances = new HashMap<Vertex, Double>();
        parents = new HashMap<Vertex, Vertex>();
        this.src = src;
        parents.put(src, null);
        distances.put(src, 0.0);
    }

    public double getDistance(Vertex v, double def) {
        if (distances.containsKey(v)) { 
            return distances.get(v);
        }
        return def;
    }
    
    public void updateVertex(Vertex v, Vertex p, double d) {
        parents.put(v, p);
        distances.put(v, d);
    }

    public LinkedList<Vertex> getPath(Vertex v) {
        LinkedList<Vertex> path = new LinkedList<Vertex>();
        Vertex next = v;
        while(parents.containsKey(next)) {
            path.addFirst(next);
            next = parents.get(next);
        }
        return path;
    }

    public Vertex getSrc() {
        return src;
    }

    public Collection<Vertex> getReached() {
        return distances.keySet();
    }
}