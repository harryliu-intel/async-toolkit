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
 * Allows transformation of device names by overriding the
 * <code>processDeviceName</code> method.  By the default, the method is the identity
 * transformation.
 **/
public class CDLDeviceNameFilter implements CDLFactoryInterface {
    protected CDLFactoryInterface inner;

    public CDLDeviceNameFilter(final CDLFactoryInterface inner) {
        this.inner = inner;
    }

    protected HierName processDeviceName(final HierName name) {
        return name;
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        inner.makeResistor(processDeviceName(name), n1, n2, val, parameters,
                           env);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        inner.makeCapacitor(processDeviceName(name), npos, nneg, val,
                            parameters, env);
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        inner.makeTransistor(processDeviceName(name), type, ns, nd, ng, nb,
                             w, l, parameters, env);
    }

    
    public void makeDiode(HierName name, String type, HierName npos,
                          HierName nneg, CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        inner.makeDiode(processDeviceName(name), type, npos, nneg, val,
                        parameters, env);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) { 
        inner.makeInductor(processDeviceName(name), npos, nneg, val,
                           parameters, env);
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        inner.makeBipolar(processDeviceName(name), type, nc, nb, ne, val,
                          parameters, env);
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        inner.makeCall(processDeviceName(name), subName, args, parameters, env);
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        inner.beginSubcircuit(subName, in, out, parameters, env);
    }
            
    public void endSubcircuit(String subName, Environment env) {
        inner.endSubcircuit(subName, env);
    }
}
