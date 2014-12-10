/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout.gdsII;

import java.util.Map;
import java.util.HashMap;


import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;

public class SharedTableEmitterFactory implements TableEmitterFactoryInterface {

    private class Emitter implements TableEmitterInterface {  

        private final String mCastCellName;
        private int refCount;
        
        private final TableEmitterInterface mRealEmitter;

        public Emitter( final TableEmitterInterface realEmitter,
                        final String castCellName ) {
            mCastCellName = castCellName;
            mRealEmitter = realEmitter;
            refCount = 0;
        }

        public boolean haveCellName( final String castName ) {
            return mRealEmitter.haveCellName( castName );
        }
        public void emitCellName( final String castName, 
                                  final String cadenceName, 
                                  final String gdsIIName ) throws TableEmitterException {
            mRealEmitter.emitCellName( castName, cadenceName, gdsIIName );
        }
        public boolean haveNodeName( final String castName ) {
            return mRealEmitter.haveNodeName( castName );
        }
        public void emitNodeName( final String castName, 
                                  final String cadenceName,
                                  final String gdsIIName ) throws TableEmitterException {
            mRealEmitter.emitNodeName( castName, cadenceName, gdsIIName );
        }
        public boolean haveInstanceName( final String castName ) {
            return mRealEmitter.haveInstanceName( castName );
        }
        public void emitInstanceName( final String castName,
                                      final String cadenceName,
                                      final String gdsIIName ) throws TableEmitterException {
            mRealEmitter.emitInstanceName( castName,
                                           cadenceName,
                                           gdsIIName );
        }
        
        public void close() throws TableEmitterException {
            assert refCount > 0;
            --refCount;
            if ( refCount == 0 ) {
                mRealEmitter.close();
                purge( mCastCellName );
            }
        }
        
        public void addRef( ) {
            ++refCount;
        }
    }

    private Map mEmitterMap;
    private TableEmitterFactoryInterface mRealEmitterFactory;

    public SharedTableEmitterFactory( final TableEmitterFactoryInterface realEmitterFactory ) {
        mRealEmitterFactory = realEmitterFactory;
        mEmitterMap = new HashMap(); 
    }

    public TableEmitterInterface getTableEmitter( final String castCellName,
                                                  final String cadenceCellName,
                                                  final String gdsIICellName )
        throws TableEmitterException {
        
        final Emitter existingEmitter =
            ( Emitter ) mEmitterMap.get( castCellName );

        final Emitter sharedEmitter;
        
        if ( existingEmitter == null ) {
            final TableEmitterInterface realEmitter =
                mRealEmitterFactory.getTableEmitter( castCellName,
                                                     cadenceCellName,
                                                     gdsIICellName );
            sharedEmitter = new Emitter( realEmitter,
                                         castCellName );
            mEmitterMap.put( castCellName, sharedEmitter );
        }
        else {
            sharedEmitter = existingEmitter;
        }
        sharedEmitter.addRef();
        return sharedEmitter;
        
    }

    private void purge( final String castCellName ) {
        mEmitterMap.remove( castCellName );
    }
}
