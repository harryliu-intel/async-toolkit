/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.util.Map;

import com.avlsi.file.common.HierName;

/**
 * A simple implementation of CDLSimpleInterface, which can be easily extended
 * by classes that wish to override a few methods.
 **/
public class CDLSimpleFilter implements CDLSimpleInterface {
    protected CDLSimpleInterface output = null;

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             double val) {
        output.makeResistor(name, n1, n2, val);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              double val) {
        output.makeCapacitor(name, npos, nneg, val);
    }

    public void makeTransistor(HierName name, int type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               double w, double l) {
        output.makeTransistor(name, type, ns, nd, ng, nb, w, l);
    }

    public void makeDiode(HierName name, int type, HierName npos, HierName nneg,
                          double w, double l, double a, double p) {
        output.makeDiode(name, type, npos, nneg, w, l, a, p);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             double val) {
        output.makeInductor(name, npos, nneg, val);
    }

    public void makeBipolar(HierName name, int type, HierName nc, HierName nb,
                            HierName ne, double a) {
        output.makeBipolar(name, type, nc, nb, ne, a);
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters) {
        output.makeCall(name, subName, args, parameters);
    }

    public void beginSubcircuit(String subName, String[] in, String[] out) {
        output.beginSubcircuit(subName, in, out);
    }

    public void endSubcircuit(String subName) {
        output.endSubcircuit(subName);
    }

    public void setOutput(final CDLSimpleInterface output) {
        this.output = output;
    }
}
