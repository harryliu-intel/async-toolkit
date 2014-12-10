/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.netlist.impl.simple;


import java.util.Map;
import java.util.List;
import java.util.Iterator;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Collections;

import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.common.HierName;

import com.avlsi.netlist.AbstractNetlist;

import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNetlistIterator;

import com.avlsi.netlist.impl.SimpleAbstractNodeIterator;
import com.avlsi.netlist.impl.SimpleAbstractDeviceIterator;
import com.avlsi.netlist.impl.SimpleAbstractNetlistIterator;

import com.avlsi.netlist.impl.simple.Net;

public final class SimpleNetlist implements AbstractNetlist{

    private final HierName m_Name;

    private final Map m_DeviceMap;
    private final Map m_NetMap;

    private final List m_Inputs;
    private final List m_Outputs;

    public SimpleNetlist( final HierName name, final String[] inputs, final String[] outputs ) {
        m_Name = name;
        m_DeviceMap = new HashMap();
        m_NetMap = new HashMap();

        m_Inputs = new ArrayList( inputs.length );
        m_Outputs = new ArrayList( outputs.length );

        int i;
        try {
            for ( i = 0 ; i < inputs.length ; ++i ) {
                final Net currNode = 
                    makeNode( HierName.makeHierName( inputs[i], '.' ) );
                m_Inputs.add( currNode );
            }

            for ( i = 0 ; i < outputs.length ; ++i ) {
                final Net currNode = 
                    makeNode( HierName.makeHierName( outputs[i], '.' ) );
                m_Outputs.add( currNode );
            }
        }
        catch ( InvalidHierNameException e ) {
            throw (IllegalArgumentException)
                new IllegalArgumentException().initCause(e);
        }

    }

    public Net makeNode( HierName name ) {
        Net ret = ( Net ) m_NetMap.get( name );
        if ( ret == null ) {
            ret = new Net( name );
            m_NetMap.put( name, ret );
        }
        return ret;
    }
    
    public void addDevice( Device newDevice ) {
        m_DeviceMap.put( newDevice.getName(), newDevice );
    }

    public int hashCode( ) {
        return getName().hashCode();
    }

    public boolean equals( final Object other ) {
        if ( other instanceof SimpleNetlist ) {
            final SimpleNetlist otherNetlist = ( SimpleNetlist ) other;
            return getName().equals( otherNetlist.getName() );
        }
        else {
            return false;
        }
    }

    public AbstractNodeIterator getInputNodes() {
        return new SimpleAbstractNodeIterator( m_Inputs.iterator() );
    }
 
    public AbstractNodeIterator getOutputNodes() {
        return new SimpleAbstractNodeIterator( m_Outputs.iterator() );
    }
 
    public HierName getName() {
        return m_Name;
    }

    public AbstractNodeIterator getNodes() {
        return new SimpleAbstractNodeIterator( m_NetMap.values().iterator() );
    }
 
    public AbstractDeviceIterator getDevices() {
        return new SimpleAbstractDeviceIterator( m_DeviceMap.values().iterator() );
    }
 
    public AbstractNetlistIterator getSubcircuits() {
        return new SimpleAbstractNetlistIterator( Collections.EMPTY_SET.iterator() );
    }


}
