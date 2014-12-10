/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.util.LinkedList;
import java.util.List;

import com.avlsi.util.container.Pair;
import com.avlsi.util.graph.Vertex;

class SlewViolation implements Comparable <SlewViolation> {
    private LinkedList<Vertex> slow;
    private LinkedList<Vertex> fast;
    private double fastDelay;
    private double slowDelay;
    private double slack;
    private Vertex input = null;

    SlewViolation(LinkedList<Vertex> slow,
                  double slowDelay,
                  LinkedList<Vertex> fast,
                  double fastDelay) {
        this.slow = slow;
        this.fast = fast;
        this.slowDelay = slowDelay;
        this.fastDelay = fastDelay;
        this.slack = fastDelay - slowDelay;
    }

    SlewViolation(LinkedList<Vertex> slow,
                  double slowDelay,
                  Vertex input,
                  LinkedList<Vertex> fast,
                  double fastDelay) {
        this.slow = slow;
        this.fast = fast;
        this.slowDelay = slowDelay;
        this.fastDelay = fastDelay;
        this.slack = fastDelay - slowDelay;
        this.input = input;
    }

    public String toString() {
        LinkedList<Vertex> slow = new LinkedList<Vertex>();
        slow.addAll(this.slow);
        if(input != null) {
            slow.addFirst(input);
        }
        return Math.round(slack) + " " + Math.round(slowDelay) + " " + 
            Math.round(fastDelay) + " " + victimLocalName() + " " 
            + slow + " " + fast;
    }

    Pair<String, String> victimLocalName() {
        final HalfOp h = (HalfOp)  slow.get(0);
        return h.getLocalName();
    }

    @Override public int compareTo(SlewViolation v) {
        if (v==null) {
            throw new NullPointerException();
        }

        if (this.equals(v)) {
            return 0;
        }

        int slkCmp = Double.compare(this.slack, v.slack);
        int slowDelayCmp = Double.compare(this.slowDelay, v.slowDelay);
        int fastDelayCmp  = Double.compare(this.fastDelay, v.fastDelay);
        
        if(slkCmp != 0 ) {
            return slkCmp;
        }
        
        if(slowDelayCmp != 0) {
            return slowDelayCmp;
        }
        
        if(fastDelayCmp != 0) {
            return fastDelayCmp;
        }
        
        int slowCmp = this.slow.toString().compareTo(v.slow.toString());
        
        if(slowCmp != 0) {
            return slowCmp;
        }

        int fastCmp = this.fast.toString().compareTo(v.fast.toString());
        assert fastCmp != 0;
        return fastCmp;
    }
}