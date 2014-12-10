/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.tools.ipgen;


import java.util.Set;
import java.util.Map;
import java.util.HashMap;

import com.avlsi.layout.gdsII.TableEmitterInterface;


public class HashMapTableEmitter implements TableEmitterInterface {

    private Map mCellCastToGDSII;
    private Map mCellCadenceToGDSII;
    private Map mCellGDSIIToCast;
    private Map mCellGDSIIToCadence;
    private HashMap mNodeCastToGDSII;
    private HashMap mNodeCadenceToGDSII;
    private HashMap mNodeGDSIIToCast;
    private HashMap mNodeGDSIIToCadence;
    private HashMap mInstanceCastToGDSII;
    private HashMap mInstanceCadenceToGDSII;
    private HashMap mInstanceGDSIIToCast;
    private HashMap mInstanceGDSIIToCadence;
    
    public HashMapTableEmitter( final Map cellCastToGDSII,
                                final Map cellCadenceToGDSII,
                                final Map cellGDSIIToCast,
                                final Map cellGDSIIToCadence ) {
        mCellCastToGDSII = cellCastToGDSII;
        mCellCadenceToGDSII = cellCadenceToGDSII;
        mCellGDSIIToCast = cellGDSIIToCast;
        mCellGDSIIToCadence = cellGDSIIToCadence;
        mNodeCastToGDSII = new HashMap();
        mNodeCadenceToGDSII = new HashMap();
        mNodeGDSIIToCast = new HashMap();
        mNodeGDSIIToCadence = new HashMap();
        mInstanceCastToGDSII = new HashMap();
        mInstanceCadenceToGDSII = new HashMap();
        mInstanceGDSIIToCast = new HashMap();
        mInstanceGDSIIToCadence = new HashMap();
        
    }

    public boolean haveCellName( final String castName ) {
        return mCellCastToGDSII.containsKey( castName );
    }

    public void emitCellName( final String castName,
                              final String cadenceName,
                              final String gdsIIName ) {
        mCellCastToGDSII.put( castName, gdsIIName );
        mCellCadenceToGDSII.put( cadenceName, gdsIIName );
        mCellGDSIIToCast.put( gdsIIName, castName );
        mCellGDSIIToCadence.put( gdsIIName, cadenceName );
    }

    public boolean haveNodeName( final String castName ) {
        return mNodeCastToGDSII.containsKey( castName );
    }

    public void emitNodeName( final String castName,
                              final String cadenceName,
                              final String gdsIIName ) {
        mNodeCastToGDSII.put( castName, gdsIIName );
        mNodeCadenceToGDSII.put( cadenceName, gdsIIName );
        mNodeGDSIIToCast.put( gdsIIName, castName );
        mNodeGDSIIToCadence.put( gdsIIName, cadenceName );
    }


    public boolean haveInstanceName( final String castName ) {
        return mInstanceCastToGDSII.containsKey( castName );
    }

    public void emitInstanceName( final String castName,
                                  final String cadenceName,
                                  final String gdsIIName ) {
        mInstanceCastToGDSII.put( castName, gdsIIName );
        mInstanceCadenceToGDSII.put( cadenceName, gdsIIName );
        mInstanceGDSIIToCast.put( gdsIIName, castName );
        mInstanceGDSIIToCadence.put( gdsIIName, cadenceName );
    }

    public Set getCastCellNames() {
        return mCellCastToGDSII.entrySet();
    }

    public Set getCastNodeNames() {
        return mNodeCastToGDSII.entrySet();
    }

    public Set getCastInstanceNames() {
        return mInstanceCastToGDSII.entrySet();
    }

    public Set getGDSCellNames() {
        return mCellGDSIIToCast.entrySet();
    }

    public Set getGDSNodeNames() {
        return mNodeGDSIIToCast.entrySet();
    }

    public Set getGDSInstanceNames() {
        return mInstanceGDSIIToCast.entrySet();
    }

    public Set getCadenceCellNames() {
        return mCellCadenceToGDSII.entrySet();
    }

    public Set getCadenceNodeNames() {
        return mNodeCadenceToGDSII.entrySet();
    }

    public Set getCadenceInstanceNames() {
        return mInstanceCadenceToGDSII.entrySet();
    }

    public String getGDSIICellNameForCastCellName( final String castName ) {
        return ( String ) mCellCastToGDSII.get( castName );
    }
    public String getGDSIICellNameForCadenceCellName( final String cadenceName ) {
        return ( String ) mCellCadenceToGDSII.get( cadenceName );
    }
    public String getCastCellNameForGDSIICellName( final String gdsIIName ) {
        return ( String ) mCellGDSIIToCast.get( gdsIIName );
    }
    public String getCadenceCellNameForGDSIICellName( final String gdsIIName ) {
        return ( String ) mCellGDSIIToCadence.get( gdsIIName );
    }
    public String getCadenceCellNameForCastCellName( final String castName ) {
        final String gdsIIName = getGDSIICellNameForCastCellName( castName );
        if ( gdsIIName != null ) {
            return getCadenceCellNameForGDSIICellName( gdsIIName );
        }
        else {
            return null;
        }
    }
    public String getCastCellNameForCadenceCellName( final String cadenceName ) {
        final String gdsIIName = getGDSIICellNameForCadenceCellName( cadenceName );
        if ( gdsIIName != null ) {
            return getCastCellNameForGDSIICellName( gdsIIName );
        }
        else {
            return null;
        }
    }

    public String getGDSIINodeNameForCastNodeName( final String castName ) {
        return ( String ) mNodeCastToGDSII.get( castName );
    }
    public String getGDSIINodeNameForCadenceNodeName( final String cadenceName ) {
        return ( String ) mNodeCadenceToGDSII.get( cadenceName );
    }
    public String getCastNodeNameForGDSIINodeName( final String gdsIIName ) {
        return ( String ) mNodeGDSIIToCast.get( gdsIIName );
    }
    public String getCadenceNodeNameForGDSIINodeName( final String gdsIIName ) {
        return ( String ) mNodeGDSIIToCadence.get( gdsIIName );
    }
    public String getCadenceNodeNameForCastNodeName( final String castName ) {
        final String gdsIIName = getGDSIINodeNameForCastNodeName( castName );
        if ( gdsIIName != null ) {
            return getCadenceNodeNameForGDSIINodeName( gdsIIName );
        }
        else {
            return null;
        }
    }
    public String getCastNodeNameForCadenceNodeName( final String cadenceName ) {
        final String gdsIIName = getGDSIINodeNameForCadenceNodeName( cadenceName );
        if ( gdsIIName != null ) {
            return getCastNodeNameForGDSIINodeName( gdsIIName );
        }
        else {
            return null;
        }
    }

    public String getGDSIIInstanceNameForCastInstanceName( final String castName ) {
        return ( String ) mInstanceCastToGDSII.get( castName );
    }
    public String getGDSIIInstanceNameForCadenceInstanceName( final String cadenceName ) {
        return ( String ) mInstanceCadenceToGDSII.get( cadenceName );
    }
    public String getCastInstanceNameForGDSIIInstanceName( final String gdsIIName ) {
        return ( String ) mInstanceGDSIIToCast.get( gdsIIName );
    }
    public String getCadenceInstanceNameForGDSIIInstanceName( final String gdsIIName ) {
        return ( String ) mInstanceGDSIIToCadence.get( gdsIIName );
    }
    public String getCadenceInstanceNameForCastInstanceName( final String castName ) {
        final String gdsIIName = getGDSIIInstanceNameForCastInstanceName( castName );
        if ( gdsIIName != null ) {
            return getCadenceInstanceNameForGDSIIInstanceName( gdsIIName );
        }
        else {
            return null;
        }
    }
    public String getCastInstanceNameForCadenceInstanceName( final String cadenceName ) {
        final String gdsIIName = getGDSIIInstanceNameForCadenceInstanceName( cadenceName );
        if ( gdsIIName != null ) {
            return getCastInstanceNameForGDSIIInstanceName( gdsIIName );
        }
        else {
            return null;
        }
    }

    public void close() {
    }

    

}
