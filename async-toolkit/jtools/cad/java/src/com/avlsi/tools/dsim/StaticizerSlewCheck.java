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


class StaticizerSlewCheck extends SlewCheck {
    private Map<String, Double> slowDelays;
    private Map<String, String> staticizerInv;

    StaticizerSlewCheck() {
        slowDelays = new HashMap<String, Double>();
        staticizerInv = new HashMap<String, String>();
    }

    void evaluateSlowPaths(HalfOpGraph circuit,
                           final Map<String, Double> slews) {
        slowDelays.putAll(getStaticizerDelay(circuit, slews));
    }

    double getSlowDelay(Vertex v) {
        Double slow = slowDelays.get(v.toString());
        if (slow == null) {
            return -1;
        }
        return slow;
    }

    private Map<String, Double> getStaticizerDelay(
        final HalfOpGraph circuit,
        final Map<String, Double> slews) {
        final Map<String, Double>  delays = new HashMap<String, Double>();
        circuit.foreachVertex(
            new UnaryPredicate<Vertex>() {
                @Override public boolean evaluate(final Vertex v) {
                    final Double slew = slews.get(v.toString());
                    // skip combinational nodes, and nodes with no slew.
                    if(slew == null || !circuit.isDynamic((HalfOp) v)) {
                        return true;
                    }
                    final boolean hasFullStaticizer =
                        circuit.hasFullStaticizer((HalfOp) v);
                    final HalfOp staticizerInverse =
                        circuit.getStaticizerInverse((HalfOp) v);
                    final HalfOpEdge se;
                    if(staticizerInverse != null) {
                        se = ((HalfOp) v).getEdge(staticizerInverse);
                    } else {
                        se = null;
                    }
                    final boolean staticizerDelayIsMeasured = 
                        (se != null) && se.hasMeasured();
                    v.foreachEdge
                        (new UnaryPredicate<Edge>() {
                            @Override public boolean evaluate(Edge e) {
                                HalfOpEdge he = (HalfOpEdge) e;
                                if (!staticizerDelayIsMeasured ||
                                    e.getSnk().equals(staticizerInverse)) {
                                    double delay = 
                                        he.getMaxDelay(HalfOpEdge.toPs(slew),
                                                       100,false);
                                    Double old = delays.get(v.toString());
                                    if(old == null || delay > old) {
                                        delays.put(v.toString(), delay);
                                        staticizerInv.put
                                            (v.toString(),
                                             e.getSnk().toString());
                                    }
                                }
                                return true;
                            }
                        });
                    return true;
                }
            });
        return delays;
    }

    void checkViolations(HalfOpGraph circuit,
                         HalfOp src,  ShortestPaths paths, 
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
            if(r.isDuplicate()) {
                continue;
            }
            if(r.equals(src) ||
               paths.getDistance(r, 
                                 Double.POSITIVE_INFINITY) > effectiveSlow) {
                continue;
            }

            Collection<HalfOp> enabledByR = r.enables();
            Collection<HalfOp> disabledByR = r.disables();

            if(enabledByR.contains(notSrc)) {
                createViolation
                    (src, r, 
                     circuit.getHalfOp(staticizerInv.get(src.toString())), 
                     notSrc, paths, slow, 
                     absMargin, multMargin);
            }

            if(disabledByR.contains(src)) {
                createViolation
                    (src, r, 
                     circuit.getHalfOp(staticizerInv.get(src.toString())), 
                     src, paths, slow, 
                     absMargin, multMargin);
            }
            if(checkIsochronicForks) {
                enabledByR.retainAll(disabledBySrc);
                for(HalfOp isochronicTarget : enabledByR) {
                    createViolation(src, r, isochronicTarget,
                                    isochronicTarget, paths, slow, absMargin, 
                                    multMargin);
                }
            }
        }
    }

}