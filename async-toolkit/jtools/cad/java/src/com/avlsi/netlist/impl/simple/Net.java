/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.netlist.impl.simple;

import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collections;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractDeviceIterator;

import com.avlsi.netlist.impl.SimpleAbstractDeviceIterator;

import com.avlsi.netlist.impl.simple.Device;


class Net implements AbstractNode {

    private List m_Names;
    private Map m_Devices;

    public Net( final HierName name ) {
        m_Names = new ArrayList();
        m_Devices = null;
        m_Names.add( name );
    }

    public boolean isAlias( final HierName name ) {
        return ( m_Names.indexOf( name ) != -1 );
    }

    public void addAlias( final HierName name ) { 
        if ( ! ( isAlias( name ) ) ) {
            m_Names.add( name );
        }
    }

    public void addDevice( final Device newDevice ) {
        if ( m_Devices == null ) {
            m_Devices = new HashMap();
        }
        m_Devices.put( newDevice.getName(), newDevice );
    }

    public void removeDevice( final Device devToRemove ) {
        if ( m_Devices != null ) {
            m_Devices.remove( devToRemove.getName() );
        }
    }

    public void setCanonicalName( final HierName name ) {
        final int existingIndex = m_Names.indexOf( name );
        if ( existingIndex != -1 ) {
            m_Names.remove( existingIndex );
        }
        m_Names.add( 0, name );
    }

    public HierName getCanonicalName() {
        return ( HierName )m_Names.get( 0 );
    }
    
    public Iterator getAliases() {
        return m_Names.iterator();
    }

    public AbstractDeviceIterator getDevices() {
        final Iterator i;
        if ( m_Devices != null ) {
            i = m_Devices.values().iterator();
        }
        else {
            i = Collections.EMPTY_SET.iterator();
        }
        return new SimpleAbstractDeviceIterator( i );
    }


}
