/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.netlist.impl.simple;

import java.util.HashMap;
import java.util.Map;
import java.util.Collections;

import com.avlsi.util.debug.Debug;

import com.avlsi.file.common.HierName;

import com.avlsi.netlist.Visitor;

import com.avlsi.netlist.impl.simple.Device;
import com.avlsi.netlist.impl.simple.Net;

class FET extends Device {

    private Net m_Drain;
    private Net m_Gate;
    private Net m_Source;
    private Net m_Bulk;
    
    private final double m_Length;
    private final double m_Width;
    private final String m_Type;
    
    private final Map m_Parameters;

    public FET( final HierName name, 
                final Net drain,
                final Net gate,
                final Net source,
                final Net bulk,
                final double length,
                final double width,
                final String type,
                final Map params ) {
        super( name );

        m_Drain = drain;
        m_Gate = gate;
        m_Source = source;
        m_Bulk = bulk;
        
        m_Length = length;
        m_Width = width;
        m_Type = type;
        m_Parameters = params;
 
        m_Drain.addDevice( this );
        m_Gate.addDevice( this );
        m_Source.addDevice( this );
        m_Bulk.addDevice( this );
    }

    public final void replaceNet( final Net oldNet, final Net newNet ) {
        boolean foundNet = false;
        if ( m_Drain == oldNet ) {
            m_Drain = newNet;
            foundNet = true;
        }
        if ( m_Gate == oldNet ) {
            m_Gate = newNet;
            foundNet = true;
        }
        if ( m_Source == oldNet ) {
            m_Source = newNet;
            foundNet = true;
        }
        if ( m_Bulk == oldNet ) {
            m_Bulk = newNet;
            foundNet = true;
        }
        Debug.assertTrue( foundNet );
        oldNet.removeDevice( this );
        newNet.addDevice( this );
    }
    
    public final void accept(final Visitor visitor) {
        final Map myParams;

        if ( m_Parameters == null ) {
            myParams = Collections.EMPTY_MAP;
        }
        else {
            myParams = new HashMap( m_Parameters );
        }
        
        visitor.genericTransistor( getName(),
                                   m_Drain,
                                   m_Gate,
                                   m_Source,
                                   m_Bulk,
                                   m_Length,
                                   m_Width,
                                   m_Type,
                                   myParams );
    }
    

    
    

}
