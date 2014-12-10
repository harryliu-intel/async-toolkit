/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import java.util.Map;
import java.util.HashMap;
import java.util.Set;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;

import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class CachingNameInterface implements CDLNameInterface {

    private final Map m_CellNameMap;
    private final Map m_NodeNameMap;
    private final Map m_DeviceNameMap;
    private final Map m_SubCellInstanceNameMap;
    private final Map m_TransistorModelNameMap;
    
    final CDLNameInterface m_RenameInterface;

    public CachingNameInterface( final CDLNameInterface renameInterface ) {
        m_RenameInterface = renameInterface;

        m_CellNameMap = new HashMap();
        m_NodeNameMap = new HashMap();
        m_DeviceNameMap = new HashMap();
        m_SubCellInstanceNameMap = new HashMap();
        m_TransistorModelNameMap = new HashMap();
    }

    protected Set getCellNameMappings( ) {
        return m_CellNameMap.entrySet();
    }
    
    protected Set getNodeNameMappings( ) {
        return m_NodeNameMap.entrySet();
    }

    protected Set getDeviceNameMappings() {
        return m_DeviceNameMap.entrySet();
    }

    protected Set getSubCellInstanceNameMappings() {
        return m_SubCellInstanceNameMap.entrySet();
    }

    protected Set getTransistorModelNameMappings() {
        return m_TransistorModelNameMap.entrySet();
    }

    protected void addCellNameMapping( final String oldCellName,
                                       final String newCellName ) {
        m_CellNameMap.put( oldCellName, newCellName );
    }

    protected void addNodeNameMapping( final String oldNodeName,
                                       final String newNodeName ) {
        m_NodeNameMap.put( oldNodeName, newNodeName );
    }

    protected void addDeviceNameMapping( final String oldDeviceName,
                                         final String newDeviceName ) {
        m_DeviceNameMap.put( oldDeviceName, newDeviceName );
    }

    protected void addSubCellInstanceNameMapping( final String oldInstanceName,
                                                  final String newInstanceName ) {
        m_SubCellInstanceNameMap.put( oldInstanceName, newInstanceName );
    }

    protected void addTransistorModelNameMapping( final String oldModelName,
                                                  final String newModelName ) {
        m_TransistorModelNameMap.put( oldModelName, newModelName );
    }

    public String renameCell( final String oldCellName ) 
        throws CDLRenameException
    {
        final String cachedName = ( String ) m_CellNameMap.get( oldCellName );
        final String ret;
        if ( cachedName == null ) {
            ret = m_RenameInterface.renameCell( oldCellName );
            addCellNameMapping( oldCellName, ret );
        }
        else {
            ret = cachedName;
        }
        return ret;
    }

    public String renameNode( final String oldNodeName ) 
        throws CDLRenameException
    {
        final String cachedName = ( String ) m_NodeNameMap.get( oldNodeName );
        final String ret;
        if ( cachedName == null ) {
            ret = m_RenameInterface.renameNode( oldNodeName );
            addNodeNameMapping( oldNodeName, ret );
        }
        else {
            ret = cachedName;
        }
        return ret;
    }

    public String renameDevice( final String oldDeviceName ) 
        throws CDLRenameException 
    {
        final String cachedName = ( String ) m_DeviceNameMap.get( oldDeviceName );
        final String ret;
        if ( cachedName == null ) {
            ret = m_RenameInterface.renameDevice( oldDeviceName );
            addDeviceNameMapping( oldDeviceName, ret );
        }
        else {
            ret = cachedName;
        }
        return ret;
    }

    public String renameSubCellInstance( final String oldInstanceName ) 
        throws CDLRenameException 
    {
        final String cachedName = 
           ( String ) m_SubCellInstanceNameMap.get( oldInstanceName );
        final String ret;
        if ( cachedName == null ) {
            ret = m_RenameInterface.renameSubCellInstance( oldInstanceName );
            addSubCellInstanceNameMapping( oldInstanceName, ret );
        }
        else {
            ret = cachedName;
        }
        return ret;
    }

    public String renameTransistorModel( final String oldModelName ) throws CDLRenameException {
        final String cachedName = 
           ( String ) m_TransistorModelNameMap.get( oldModelName );
        final String ret;
        if ( cachedName == null ) {
            ret = m_RenameInterface.renameTransistorModel( oldModelName );
            addTransistorModelNameMapping( oldModelName, ret );
        }
        else {
            ret = cachedName;
        }
        return ret;
    }

}
