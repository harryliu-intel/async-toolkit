/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import com.avlsi.util.graph.Edge;

class HalfOpEdge implements Edge {
    private final Rule r;
    private final HalfOp src;
    private final HalfOp snk;

    public HalfOpEdge(HalfOp src, HalfOp snk, Rule r) {
        this.r = r;
        this.src = src;
        this.snk = snk;
    }

    @Override public HalfOp getSnk() {
        return snk;
    }

    @Override public HalfOp getSrc() {
        return src;
    }

    @Override public double getWeight() {
        return getWeight(false);
    }

    public double getWeight(boolean updateSlew) {
        // TODO(piyush): Edges with no measured delay are ignored.
        //               Make this controlled by a switch.
        //               Warn when ignoring an edge.
        return getDelay(src.getSlew(), Double.POSITIVE_INFINITY,
                        updateSlew);
    }

    public double getDelay(double inputSlew, double fallbackDelay,
                           boolean updateSlew) {
        return computeDelay(inputSlew, fallbackDelay, updateSlew, false);
    }

    public double getMaxDelay(double inputSlew, double fallbackDelay,
                           boolean updateSlew) {
        return computeDelay(inputSlew, fallbackDelay, updateSlew, true);
    }

    private double computeDelay(double inputSlew, double fallbackDelay,
                                boolean updateSlew, boolean maxDelay) {
        float[] delaySlew = new float[2];
 
        src.setSlew(inputSlew);
        delaySlew[0] = (float) fallbackDelay;
        if (!r.getMeasured(src.getNode(), delaySlew)) {
            delaySlew[0] = (float) fallbackDelay;
        }
   
        if(delaySlew[0] == 0.0) {
            System.err.println("WARNING: 0 delay for edge: " + toString());
        }

        if(updateSlew) {
            snk.setSlew(delaySlew[1]);
        }
        return (maxDelay ? delaySlew[0] + toDSim(delaySlew[1]) : delaySlew[0]);
    }

    @Override public double relax(double srcWeight, double currSnkWeight) {
        double newSnkWeight = srcWeight + getWeight();
        if (newSnkWeight < currSnkWeight) {
            getWeight(true);
        } else {
            newSnkWeight = -1;
        }
        return newSnkWeight;
    }

    public double getSlew(double inputSlew, double fallbackSlew) {
        float[] delaySlew = new float[2];

        src.setSlew(inputSlew);
        delaySlew[0] = r.delay;
        delaySlew[1] = (float) fallbackSlew;
        if (!r.getMeasured(src.getNode(), delaySlew)) {
            delaySlew[1] = (float) fallbackSlew;
        }

        return delaySlew[1];
    }

    public static double toPs(double dsimUnits) {
        return dsimUnits / 100;
    }

    public static double toDSim(double ps) {
        return ps * 100;
    }

    @Override public String toString() {
        return src + "->" + snk;
    }

    Rule getRule() {
        return r;
    }

    boolean hasMeasured() {
        float[] measured = new float[2];
        return r.getMeasured(getSrc().getNode(), measured);
    }
}