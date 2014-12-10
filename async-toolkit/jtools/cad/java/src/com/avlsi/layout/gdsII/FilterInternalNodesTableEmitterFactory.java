/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout.gdsII;

import java.lang.String;
import java.lang.Boolean;

import java.util.Set;
import java.util.HashSet;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;

import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;

public final class FilterInternalNodesTableEmitterFactory implements TableEmitterFactoryInterface {

    private static final class Emitter implements TableEmitterInterface {
        private final CadenceInfo mCadenceInfo;
        private final TableEmitterInterface mRealEmitter;

        private final Set mInternalNodes;

        public Emitter( final CadenceInfo cadenceInfo,
                        final TableEmitterInterface realEmitter ) {
            mCadenceInfo = cadenceInfo;
            mRealEmitter = realEmitter;
            mInternalNodes = new HashSet();
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
            return mInternalNodes.contains( castName ) || mRealEmitter.haveNodeName( castName );
        }
        public void emitNodeName( final String castName, 
                                  final String cadenceName,
                                  final String gdsIIName ) throws TableEmitterException {
            assert mCadenceInfo != null;
            if ( ! haveNodeName( castName ) ) {
                try {
                    final Boolean inPortList =
                        ( Boolean ) mCadenceInfo.getPortNodes().getValue( HierName.makeHierName( castName, '.' ) );
                    
                    if ( ( inPortList != null ) &&
                         ( inPortList.booleanValue() ) ) {
                        mRealEmitter.emitNodeName( castName, cadenceName, gdsIIName );
                    }
                    else {
                        mInternalNodes.add( castName );
                    }
                }
                catch ( InvalidHierNameException e ) {
                    throw new TableEmitterException( "Unable to get HierName for \"" + castName + "\".", e );
                }
            }
        }
        public boolean haveInstanceName( final String castName ) {
            return mRealEmitter.haveInstanceName( castName );
        }
        public void emitInstanceName( final String castName,
                                      final String cadenceName,
                                      final String gdsIIName ) throws TableEmitterException {
            mRealEmitter.emitInstanceName( castName, cadenceName, gdsIIName );
        }

        public void close() throws TableEmitterException {
            mRealEmitter.close();
        }


    }

    private final Cadencize mCadencizer;
    private final TableEmitterFactoryInterface mRealEmitterFactory;

    public FilterInternalNodesTableEmitterFactory( final Cadencize cadencizer,
                                                   final TableEmitterFactoryInterface realEmitterFactory ) {
        mCadencizer = cadencizer;
        mRealEmitterFactory = realEmitterFactory;

    }
                                                  
    public TableEmitterInterface getTableEmitter( final String castCellName,
                                                  final String cadenceCellName,
                                                  final String gdsIICellName )
        throws TableEmitterException {
        
        final CadenceInfo cadenceInfo;
        assert mCadencizer != null;
        cadenceInfo = mCadencizer.getExistingCadenceInfo( castCellName );
        assert cadenceInfo != null;
        
        final TableEmitterInterface realEmitter = mRealEmitterFactory.getTableEmitter( castCellName,
                                                                                       cadenceCellName,
                                                                                       gdsIICellName );
        final TableEmitterInterface emitter = new Emitter( cadenceInfo,
                                                           realEmitter );
        return emitter;
    }
}
