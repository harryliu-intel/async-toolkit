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

public class InternalNodeFilterFactory extends CDLFactoryAdaptor {
    private final CDLFactoryInterface out;

    public InternalNodeFilterFactory(final CDLFactoryInterface out) {
        this.out = out;
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        this.out.makeCall(name, subName, new HierName[] {}, parameters, env);
    }

    public  void beginSubcircuit(String subName, String[] in, String[] out,
                                 Map parameters, Environment env) {
        this.out.beginSubcircuit(subName, in, out, parameters, env);
    }
    
    public void endSubcircuit(String subName, Environment env) {
        this.out.endSubcircuit(subName, env);
    }
}
