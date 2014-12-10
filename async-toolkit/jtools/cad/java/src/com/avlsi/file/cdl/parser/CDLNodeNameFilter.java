/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.file.cdl.parser;

import java.util.Map;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;

/**
 * Allows transformation of node names by overriding the
 * <code>processNodeName</code> method.  By the default, the method is the identity
 * transformation.
 **/
public class CDLNodeNameFilter implements CDLFactoryInterface {
    protected CDLFactoryInterface inner;

    public CDLNodeNameFilter(final CDLFactoryInterface inner) {
        this.inner = inner;
    }

    protected HierName processNodeName(final HierName node) {
        return node;
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        inner.makeResistor(name, processNodeName(n1), processNodeName(n2), val,
                           parameters, env);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        inner.makeCapacitor(name, processNodeName(npos), processNodeName(nneg),
                            val, parameters, env);
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        inner.makeTransistor(name, type,
                             processNodeName(ns), processNodeName(nd),
                             processNodeName(ng), processNodeName(nb),
                             w, l, parameters, env);
    }

    
    public void makeDiode(HierName name, String type, HierName npos,
                          HierName nneg, CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        inner.makeDiode(name, type,
                        processNodeName(npos), processNodeName(nneg), val,
                        parameters, env);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) { 
        inner.makeInductor(name, processNodeName(npos), processNodeName(nneg),
                           val, parameters, env);
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        inner.makeBipolar(name, type, processNodeName(nc), processNodeName(nb),
                          processNodeName(ne), val, parameters, env);
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        final HierName[] processed = new HierName[args.length];
        for (int i = 0; i < args.length; ++i) {
            processed[i] = processNodeName(args[i]);
        }
        inner.makeCall(name, subName, processed, parameters, env);
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        inner.beginSubcircuit(subName, in, out, parameters, env);
    }
            
    public void endSubcircuit(String subName, Environment env) {
        inner.endSubcircuit(subName, env);
    }
}
