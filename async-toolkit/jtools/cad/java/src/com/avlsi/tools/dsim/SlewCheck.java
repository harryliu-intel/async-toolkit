/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import com.avlsi.util.container.Pair;
import com.avlsi.util.graph.ShortestPaths;
import com.avlsi.util.graph.Vertex;

abstract class SlewCheck implements Serializable{
    private final SortedSet<SlewViolation> violations;
    
    SlewCheck(){
        violations = new TreeSet<SlewViolation> ();
    }

    abstract void evaluateSlowPaths(HalfOpGraph circuit, 
                                    final Map<String, Double> slews);

    abstract double getSlowDelay(Vertex v);

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
                createViolation(src, r, null, notSrc, paths, slow, 
                                absMargin, multMargin);
            }

            if(disabledByR.contains(src)) {
                createViolation(src, r, null, src, paths, slow, 
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

    void createViolation(HalfOp src,HalfOp reached, HalfOp slowEndPt,
                         HalfOp fastEndPt, ShortestPaths paths, double slew,
                         double absMargin, double multMargin) {
        LinkedList<Vertex> slow = new LinkedList<Vertex>();
        slow.add(src);
        if(slowEndPt != null) {
            slow.addLast(slowEndPt);
        }

        LinkedList<Vertex> fast =  paths.getPath(reached);
        if(fastEndPt != null) {
            fast.addLast(fastEndPt);
        }

        violations.add(new SlewViolation
                       (slow, slew+absMargin, fast, 
                        paths.getDistance(reached, -1)*multMargin));
    }

    void createViolation(HalfOp src,HalfOp reached, HalfOp slowEndPt, 
                         HalfOp input,
                         HalfOp fastEndPt, ShortestPaths paths, double slew,
                         double absMargin, double multMargin) {
        LinkedList<Vertex> slow = new LinkedList<Vertex>();
        slow.add(src);
        if(slowEndPt != null) {
            slow.addLast(slowEndPt);
        }

        LinkedList<Vertex> fast =  paths.getPath(reached);
        if(fastEndPt != null) {
            fast.addLast(fastEndPt);
        }

        SlewViolation v = new SlewViolation(slow, slew+absMargin, input, fast, 
                                            paths.getDistance(reached, -1)*multMargin);
        violations.add(v);
    }

    static double computeEffectiveDelay(double slow, double absMargin, 
                                 double multMargin) {
        return (slow+absMargin)/multMargin;
    }

    boolean printViolations(PrintWriter pw, 
                         boolean reportVictimsOnce) {
        HashSet<Pair<String, String> > printed = null;
        if(reportVictimsOnce) {
            printed = new  HashSet<Pair<String, String> >();
        }
        pw.println(violations.isEmpty() ? "PASS":"FAIL");
        for (SlewViolation v: violations) {
            Pair<String, String> victim = v.victimLocalName();
            if(!reportVictimsOnce || !printed.contains(victim)) {
                pw.println(v.toString());
                if(reportVictimsOnce) {
                    printed.add(victim);
                }
            }
        }
        return violations.isEmpty();
    }
    
}