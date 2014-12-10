/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl.simple;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.ListIterator;

import com.avlsi.util.debug.Debug;

import com.avlsi.file.common.HierName;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;

import com.avlsi.netlist.impl.SimpleAbstractNodeIterator;

import com.avlsi.netlist.impl.simple.SimpleNetlist;
import com.avlsi.netlist.impl.simple.Device;
import com.avlsi.netlist.impl.simple.Net;

class SubCircuit extends Device {
    private final SimpleNetlist m_Circuit;
    private final List m_Nodes;
    private Map m_Params;

    public SubCircuit( final HierName name, 
                       final SimpleNetlist circuit,
                       final List nodes ) {
        super( name );
        m_Circuit = circuit;
        m_Nodes = nodes;
        m_Params = null;
        
        final ListIterator i = m_Nodes.listIterator();
        
        while ( i.hasNext() ) {
            final Net currNet = ( Net ) i.next() ;
            currNet.addDevice( this );
        }
    }

    public SubCircuit( final HierName name,
                       final SimpleNetlist circuit,
                       final List nodes,
                       final Map params ) {
        this( name, circuit, nodes );
        m_Params = params;
    }

    public final void accept(final Visitor visitor) {
        final Map myParams;

        if ( m_Params == null ) {
            myParams = Collections.EMPTY_MAP;
        }
        else {
            myParams = new HashMap( m_Params );
        }
        
        visitor.subcircuitCall( getName(),
                                m_Circuit,
                                new SimpleAbstractNodeIterator(m_Nodes.iterator() ),
                                myParams );
    }

    public final void replaceNet( final Net oldNet, final Net newNet ) {
        boolean foundNet = false;
        final ListIterator nodeIter =  m_Nodes.listIterator();

        while ( nodeIter.hasNext() ) {
            final Net currNet = ( Net ) nodeIter.next();
            if ( currNet == oldNet ) {
                nodeIter.set( newNet );
                foundNet = true;
            }
        }
        Debug.assertTrue( foundNet );
        oldNet.removeDevice( this );
        newNet.addDevice( this );
    }
}
