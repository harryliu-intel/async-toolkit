/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

import java.util.Collection;

import com.avlsi.util.functions.UnaryPredicate;

public interface Vertex extends Comparable<Vertex> {
    public void foreachEdge(final UnaryPredicate<Edge> func);
    public String toString();
    public Vertex duplicate();
    public void deleteEdge(Vertex v);
    public void addEdge(Vertex v);
    public void restore();
}