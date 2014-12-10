/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.parser;

import java.util.Set;
import java.util.Map;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;

public final class CellSetFactory extends CDLFactoryAdaptor {
    private final Set mCellSet;

    public CellSetFactory( final Set targetSet ){
        mCellSet = targetSet;
    }
    public  void beginSubcircuit(String subName, String[] in, String[] out,
                                 Map parameters, Environment env) {
        mCellSet.add( subName );
    }
}
