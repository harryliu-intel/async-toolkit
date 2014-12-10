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

import com.avlsi.file.cdl.parser.CDLFactoryInterface;

public class CDLFactoryAdaptor implements CDLFactoryInterface {

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters, Environment env) {
    }
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters, Environment env) {
                
    }

    public void makeTransistor(HierName name, String type, HierName ns, HierName nd,
                               HierName ng, HierName nb, CDLLexer.InfoToken w,
                               CDLLexer.InfoToken l, Map parameters, Environment env) {
    }

    
    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val, Map parameters, Environment env) {
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters, Environment env) { 
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
    }

    
    public  void beginSubcircuit(String subName, String[] in, String[] out,
                                 Map parameters, Environment env) {
    }
            
            
    public void endSubcircuit(String subName, Environment env) {
    }
}
