/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

public interface Edge {
    public Vertex getSnk();
    public Vertex getSrc();
    public double getWeight();
    public double relax(double srcWeight, double currSnkWeight);
}