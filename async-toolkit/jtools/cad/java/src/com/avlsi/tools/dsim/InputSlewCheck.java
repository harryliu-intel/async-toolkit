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

import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.graph.ShortestPaths;
import com.avlsi.util.graph.Edge;
import com.avlsi.util.graph.Vertex;

class InputSlewCheck extends SlewCheck {
    private Map<String, Double> slowDelays;
    private Map<String, String> criticalInput;

    InputSlewCheck() {
        slowDelays = new HashMap<String, Double>();
        criticalInput = new HashMap<String, String>();
    }

    void evaluateSlowPaths(HalfOpGraph circuit,
                           final Map<String, Double> slews) {
        slowDelays.putAll(maxInputSlew(circuit, slews));

    }

    double getSlowDelay(Vertex v) {
        Double slow = slowDelays.get(v.toString());
        if (slow == null) {
            return -1;
        }
        return slow;
    }

    private Map<String,Double> maxInputSlew(
        final HalfOpGraph circuit,
        final Map<String,Double> slews) {
        final HashMap<String,Double> maxInputSlews = 
            new HashMap<String, Double>();
        circuit.foreachVertex(
            new UnaryPredicate<Vertex> () {
                @Override public boolean evaluate(Vertex v) {
                    v.foreachEdge(
                        new UnaryPredicate<Edge>() {
                            @Override public boolean evaluate(Edge e) {
                                String srcKey = e.getSrc().toString();
                                String snkKey = e.getSnk().toString();
                                Double inputSlew = slews.get(srcKey) ;
                                Double victimSlew = maxInputSlews.get(snkKey);
                                if(inputSlew != null && 
                                   (victimSlew == null ||
                                    inputSlew > victimSlew)) {
                                    maxInputSlews.put(snkKey, inputSlew);
                                    criticalInput.put(snkKey.toString(), 
                                                      e.getSrc().toString());
                                }
                                return true;
                            }
                        });
                    return true;
                }
            });
        return maxInputSlews;
    }

    @Override 
    void checkViolations(HalfOpGraph circuit, HalfOp src, ShortestPaths paths, 
                         double absMargin, double multMargin, 
                         boolean checkIsochronicForks) {
        Collection<Vertex> reached =  paths.getReached();
        Collection<HalfOp> disabledBySrc = src.disables();
        HalfOp notSrc = src.not();
        double slow = getSlowDelay(src);
        if (slow < 0 ) {
            return;
        }
        double effectiveSlow = computeEffectiveDelay(slow, absMargin, 
                                                     multMargin);
        for(Vertex v : reached) {
            HalfOp r = (HalfOp) v;

            if(r.equals(src) || r.isDuplicate() ||
               paths.getDistance(r, 
                                 Double.POSITIVE_INFINITY) > effectiveSlow) {
                continue;
            }

            if(r.equals(notSrc)) {
                //TODO(piyush): This check is "optimistic", perhaps it shoud
                // check that notSrc is enabled and substract the fast
                // delay through source from the cut-off path?
                createViolation
                    (src, r, null, 
                     circuit.getHalfOp(criticalInput.get(src.toString())), 
                     null, paths, slow, absMargin, multMargin);
            }

            // TODO(piyush): implement isochronic fork check here.
            assert(!checkIsochronicForks);
        }
    }
}