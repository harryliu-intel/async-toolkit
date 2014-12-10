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
import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.TableEmitterException;

import com.avlsi.tools.ipgen.HashMapTableEmitter;

public class HashMapTableEmitterFactory implements TableEmitterFactoryInterface {

    private final HashMap mCellToEmitterMap;
    private final Map mCellCastToGDSII;
    private final Map mCellCadenceToGDSII;
    private final Map mCellGDSIIToCast;
    private final Map mCellGDSIIToCadence;


    public HashMapTableEmitterFactory() {
        mCellToEmitterMap = new HashMap( );
        mCellCastToGDSII = new HashMap();
        mCellCadenceToGDSII = new HashMap();
        mCellGDSIIToCast = new HashMap();
        mCellGDSIIToCadence = new HashMap();
    }

    public TableEmitterInterface getTableEmitter( final String castCellName,
                                                  final String cadenceCellName,
                                                  final String gdsIICellName ) 
        throws TableEmitterException
    {

        final HashMapTableEmitter newEmitter = new HashMapTableEmitter( mCellCastToGDSII, 
                                                                        mCellCadenceToGDSII,
                                                                        mCellGDSIIToCast, 
                                                                        mCellGDSIIToCadence );
        newEmitter.emitCellName( castCellName, cadenceCellName, gdsIICellName );
        mCellToEmitterMap.put( castCellName, newEmitter );
        return newEmitter;    
    }

    public HashMapTableEmitter getEmitterForCell( final String castCellName ) {
        return ( HashMapTableEmitter ) mCellToEmitterMap.get( castCellName );
    }

    public final String getGDSIICellNameForCastCellName( final String castCellName ) {
    
        return ( String ) mCellCastToGDSII.get( castCellName );
    }

    public final String getCadenceCellNameForCastCellName( final String castCellName ) {
        final String gdsIIName = getGDSIICellNameForCastCellName( castCellName );

        if ( gdsIIName != null ) {
            return ( String ) mCellGDSIIToCadence.get( gdsIIName );
        }
        else {
            return null ;
        }
    }
                                                 
    
    

}
