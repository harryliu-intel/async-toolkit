/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.DeviceTypes;

public abstract class CDLInterfaceSimplifier implements CDLFactoryInterface, CDLSimpleInterface {
    /* These methods from CDLSimpleInterface must be implemented by any
     * subclass. */
    public abstract void makeResistor(HierName name, HierName n1,
                                      HierName n2, double val);
    public abstract void makeCapacitor(HierName name, HierName npos,
                                       HierName nneg, double val);
    public abstract void makeTransistor(HierName name, int type, HierName ns,
                                        HierName nd, HierName ng, HierName nb,
                                        double w, double l);
    public abstract void makeDiode(HierName name, int type, HierName npos,
                                   HierName nneg, double w, double l, double a, double p );
    public abstract void makeInductor(HierName name, HierName npos,
                                      HierName nneg, double val);
    public abstract void makeBipolar(HierName name, int type, HierName nc,
                                     HierName nb, HierName ne, double a);
    public abstract void makeCall(HierName name, String subName,
                                  HierName[] args, Map parameters);
    public abstract void beginSubcircuit(String subName, String[] in,
                                         String[] out);
    public abstract void endSubcircuit(String subName);

    private Environment currentEnv;

    public CDLInterfaceSimplifier() {
        currentEnv = new LocalEnvironment();
    }

    /* These methods are from CDLFactoryInterface.  They simply invoke the
     * simpler methods defined in CDLSimpleInterface. */
    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        final double res = getValue(val, env);
        makeResistor(name, n1, n2, 1 / res);
    }
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        final double cap = getValue(val, env);
        makeCapacitor(name, npos, nneg, cap);
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        final double wid = getValue(w, env);
        final double len = getValue(l, env);

        int typ;
        if (type.startsWith("p") || type.startsWith("P")) {
            typ = DeviceTypes.P_TYPE;
        } else {
            typ = DeviceTypes.N_TYPE;
        }

        makeTransistor(name, typ, ns, nd, ng, nb, wid, len);
    }

    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        final double a = getValue(val, env);
        final double w = Math.sqrt(a);
        final double l = w;
        final double p = 2*w + 2*l;

        int typ;
        if (type.equals("DW") || type.equals("dw")) {
            typ = DeviceTypes.N_TYPE;
        } else {
            typ = DeviceTypes.P_TYPE;
        }
        

        makeDiode(name, typ, npos, nneg, w,l,a,p);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        final double ind = getValue(val, env);
        makeCapacitor(name, npos, nneg, ind);
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        int typ;
        if (type.startsWith("p") || type.startsWith("P")) {
            typ = DeviceTypes.P_TYPE;
        } else {
            typ = DeviceTypes.N_TYPE;
        }
        makeBipolar(name, typ, nc, nb, ne, getValue(val, env));
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        makeCall(name, subName, args, resolveCallParameter(parameters, env));
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        beginSubcircuit(subName, in, out);
    }

    public void endSubcircuit(String subName, Environment env) {
        endSubcircuit(subName);
    }
    
    public static Map resolveCallParameter(Map parameters, Environment env) {
        Map result = new HashMap();
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final CDLLexer.InfoToken val = (CDLLexer.InfoToken) entry.getValue();
            result.put(entry.getKey(), val.getValue(env));
        }
        return result;
    }

    public static double getValue(final CDLLexer.InfoToken val,
                                  final Environment env) {
        if (val == null) return Double.NaN;

        final Double result = val.getValue(env);
        if (result == null) {
            throw new RuntimeException("Cannot evaluate floating point expression: " + val);
        }
        return result.doubleValue();
    }
}
