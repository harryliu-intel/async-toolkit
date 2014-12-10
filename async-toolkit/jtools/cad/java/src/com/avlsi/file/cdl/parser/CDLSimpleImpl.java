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
public class CDLSimpleImpl implements CDLSimpleInterface {
    public void makeResistor(HierName name, HierName n1, HierName n2,
                             double val) { }
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              double val) { }

    public void makeTransistor(HierName name, int type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               double w, double l) { }

    public void makeDiode(HierName name, int type, HierName npos, HierName nneg,
                          double w, double l, double a, double p) { }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             double val) { }

    public void makeBipolar(HierName name, int type, HierName nc, HierName nb,
                            HierName ne, double a) { }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters) { }

    public void beginSubcircuit(String subName, String[] in, String[] out) { }

    public void endSubcircuit(String subName) { }
}
