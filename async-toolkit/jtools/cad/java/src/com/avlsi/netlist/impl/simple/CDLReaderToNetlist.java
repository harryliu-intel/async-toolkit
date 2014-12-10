/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.netlist.impl.simple;


import java.io.Reader;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;

import com.avlsi.netlist.AbstractNetlist;

import com.avlsi.netlist.impl.simple.SimpleNetlistFactory;



import antlr.collections.AST;

public class CDLReaderToNetlist {

    public static SimpleNetlistFactory readCDL( final Reader r ) 
    throws RecognitionException, TokenStreamException 
    {
        final SimpleNetlistFactory factory = new SimpleNetlistFactory();
        ReadCDLIntoFactory.readCDL( r, factory );
        return factory;
    }


    public static AbstractNetlist readCDL( final Reader r, 
                                           final String rootCellName )
    throws RecognitionException, TokenStreamException 
    {
        
        final SimpleNetlistFactory factory = readCDL( r );
        
        final AbstractNetlist ret = factory.getNetlist( rootCellName );
        
        return ret;
    }

}
