/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

import com.avlsi.util.functions.UnaryPredicate;

public interface Graph {
    public void foreachVertex(final UnaryPredicate<Vertex> func);
}