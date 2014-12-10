/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.util.Map;


import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;

public final class SplittingFactory implements CDLFactoryInterface {

    private CDLFactoryInterface mFirst;
    private CDLFactoryInterface mSecond;


    public SplittingFactory( CDLFactoryInterface first, CDLFactoryInterface second ) {
        mFirst = first;
        mSecond = second;
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters, Environment env) {
        mFirst.makeResistor( name, n1, n2, val, parameters, env );
        mSecond.makeResistor( name, n1, n2, val, parameters, env );
    }

    
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters, Environment env) {
        mFirst.makeCapacitor( name, npos, nneg, val, parameters, env );
        mSecond.makeCapacitor( name, npos, nneg, val, parameters, env );
    }

    
    public void makeTransistor(HierName name, String type, HierName ns, HierName nd,
                               HierName ng, HierName nb, CDLLexer.InfoToken w,
                               CDLLexer.InfoToken l, Map parameters, Environment env) {
        mFirst.makeTransistor( name, type, ns, nd, ng, nb, w, l, parameters, env );
        mSecond.makeTransistor( name, type, ns, nd, ng, nb, w, l, parameters, env );
    }

    
    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val, Map parameters,
                          Environment env) {
        mFirst.makeDiode( name, type, npos, nneg, val, parameters, env );
        mSecond.makeDiode( name, type, npos, nneg, val, parameters, env );
    }

    
    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters, Environment env) { 
        mFirst.makeInductor( name, npos, nneg, val, parameters, env );
        mSecond.makeInductor( name, npos, nneg, val, parameters, env );
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne,
                            CDLLexer.InfoToken val, Map parameters,
                            Environment env) {
        mFirst.makeBipolar( name, type, nc, nb, ne, val, parameters, env );
        mSecond.makeBipolar( name, type, nc, nb, ne, val, parameters, env );
    }
    
    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        mFirst.makeCall( name, subName, args, parameters, env );
        mSecond.makeCall( name, subName, args, parameters, env );
    }

    
    public  void beginSubcircuit(String subName, String[] in, String[] out,
                                 Map parameters, Environment env) {
        mFirst.beginSubcircuit( subName, in, out, parameters, env );
        mSecond.beginSubcircuit( subName, in, out, parameters, env ); 
    }

   
    public void endSubcircuit(String subName, Environment env) {
        mFirst.endSubcircuit( subName, env );
        mSecond.endSubcircuit( subName, env );
    }


}
