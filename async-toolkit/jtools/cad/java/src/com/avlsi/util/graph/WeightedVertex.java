/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

import java.lang.NullPointerException;

class WeightedVertex implements Comparable<WeightedVertex> {
    private final Vertex v;
    private double weight;

    public WeightedVertex(Vertex v, double w) {
        this.v = v;
        this.weight = w;
    }

    public double getWeight() {
        return this.weight;
    }

    public void setWeight(double w) {
        this.weight = w;
    }

    public Vertex getVertex() {
        return v;
    }

    @Override public int compareTo(WeightedVertex wv) {
        if(wv == null) {
            throw new NullPointerException();
        }

        if(this.weight != wv.weight) {
            return (this.weight < wv.weight) ? -1 : 1;
        }

        return this.v.compareTo(wv.getVertex());
    }

    @Override public boolean equals(Object o) {
        return (o != null) && (o instanceof WeightedVertex) && 
            this.compareTo((WeightedVertex) o) == 0;       
    }
}