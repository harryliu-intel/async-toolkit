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
import java.util.Map;

import com.avlsi.util.graph.ShortestPaths;
import com.avlsi.util.graph.Vertex;

class OutputSlewCheck extends SlewCheck {
    private Map<String, Double> slowDelays;

    OutputSlewCheck() {
        slowDelays = new HashMap<String, Double>();
    }

    void evaluateSlowPaths(HalfOpGraph circuit,
                           final Map<String, Double> slews) {
        slowDelays.putAll(slews);
    }

    double getSlowDelay(Vertex v) {
        Double slow = slowDelays.get(v.toString());
        if (slow == null) {
            return -1;
        }
        return slow;
    }
}