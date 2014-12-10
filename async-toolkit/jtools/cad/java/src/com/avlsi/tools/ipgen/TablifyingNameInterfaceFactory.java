/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.ipgen;


import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.TableEmitterException;


public class TablifyingNameInterfaceFactory implements CDLNameInterfaceFactory {

    private static final class TablifyingNameInterface implements CDLNameInterface {
    
        private final CDLNameInterface mCadenceNameInterface;
        private final CDLNameInterface mGDSIINameInterface;
        
        private final TableEmitterInterface mTableEmitter;

        public TablifyingNameInterface( final CDLNameInterface cadenceNameInterface,
                                        final CDLNameInterface gdsIINameInterface,
                                        final TableEmitterInterface tableEmitter ) {
         
            mCadenceNameInterface = cadenceNameInterface;
            mGDSIINameInterface = gdsIINameInterface;
            mTableEmitter = tableEmitter;

        }

        public String renameCell(String name ) 
            throws CDLRenameException 
        {
            final String gdsIICellName = mGDSIINameInterface.renameCell( name );
            if ( ! ( mTableEmitter.haveCellName( name ) ) ) {
                final String cadenceCellName = mCadenceNameInterface.renameCell( name );
                try {
                    mTableEmitter.emitCellName( name, cadenceCellName, gdsIICellName );
                }
                catch ( TableEmitterException e ) {
                    throw new CDLRenameException( e );
                }
            }
            return gdsIICellName;
        }
        
        public String renameNode( final String oldNodeName ) 
            throws CDLRenameException
        {
            
            final String gdsIINodeName = mGDSIINameInterface.renameNode( oldNodeName );
            if ( ! ( mTableEmitter.haveNodeName( oldNodeName ) ) ) {
                final String cadenceNodeName = mCadenceNameInterface.renameNode( oldNodeName );
                try {
                    mTableEmitter.emitNodeName( oldNodeName, cadenceNodeName, gdsIINodeName );
                }
                catch ( TableEmitterException e ) {
                    throw new CDLRenameException( e );
                }
            }
            return gdsIINodeName;
            
        }
        
        public String renameDevice( final String oldDeviceName ) 
            throws CDLRenameException 
        {
            final String gdsIIDeviceName = mGDSIINameInterface.renameDevice( oldDeviceName );
            if ( ! ( mTableEmitter.haveInstanceName( oldDeviceName ) ) ) {
                final String cadenceDeviceName = mCadenceNameInterface.renameDevice( oldDeviceName );
                try {
                    mTableEmitter.emitInstanceName( oldDeviceName, cadenceDeviceName, gdsIIDeviceName );
                }
                catch ( TableEmitterException e ) {
                    throw new CDLRenameException( e );
                }
            }
            return gdsIIDeviceName;
        }
        
        public String renameSubCellInstance( final String oldInstanceName ) 
            throws CDLRenameException
        {
            final String gdsIIInstanceName = mGDSIINameInterface.renameSubCellInstance( oldInstanceName );
            if ( ! ( mTableEmitter.haveInstanceName( oldInstanceName ) ) ) {
                final String cadenceInstanceName = mCadenceNameInterface.renameSubCellInstance( oldInstanceName );
                try {
                    mTableEmitter.emitInstanceName( oldInstanceName, cadenceInstanceName, gdsIIInstanceName );
                }
                catch ( TableEmitterException e ) {
                    throw new CDLRenameException( e );
                }
            }
            return gdsIIInstanceName;
        }
        
        public String renameTransistorModel( final String oldTransistorModel )
            throws CDLRenameException
        {
            return mGDSIINameInterface.renameTransistorModel( oldTransistorModel );
        }
        
        public void close() throws TableEmitterException {
            mTableEmitter.close();  
        }
    }

    private final TableEmitterFactoryInterface mTableEmitterFactory;
    private final CDLNameInterfaceFactory mCadenceNameInterfaceFactory;
    private final CDLNameInterfaceFactory mGDSIINameInterfaceFactory;

    private TablifyingNameInterface mCurrNameInterface;

    public TablifyingNameInterfaceFactory( final TableEmitterFactoryInterface tableEmitterFactory,
                                           final CDLNameInterfaceFactory cadenceNameInterfaceFactory,
                                           final CDLNameInterfaceFactory gdsIINameInterfaceFactory ) {
        mTableEmitterFactory = tableEmitterFactory;
        mCadenceNameInterfaceFactory = cadenceNameInterfaceFactory;
        mGDSIINameInterfaceFactory = gdsIINameInterfaceFactory;
        mCurrNameInterface = null;
    }


    public CDLNameInterface getNameInterface( final String cellName ) 
        throws CDLRenameException
    {
        
        final CDLNameInterface cadenceNameInterface = mCadenceNameInterfaceFactory.getNameInterface( cellName );
        final CDLNameInterface gdsIINameInterface = mGDSIINameInterfaceFactory.getNameInterface( cellName );

        final String cadenceCellName = cadenceNameInterface.renameCell( cellName );
        final String gdsIICellName = gdsIINameInterface.renameCell( cellName );

        try {
            close();
            final TableEmitterInterface tableEmitter = mTableEmitterFactory.getTableEmitter( cellName,
                                                                                             cadenceCellName,
                                                                                             gdsIICellName );
            mCurrNameInterface = new TablifyingNameInterface( cadenceNameInterface,
                                                              gdsIINameInterface,
                                                              tableEmitter );
            return mCurrNameInterface;
        }
        catch ( TableEmitterException e ) {
            throw new CDLRenameException( e );
        }

    }

    public final void close() throws TableEmitterException {
        if ( mCurrNameInterface != null ) {
            mCurrNameInterface.close();
            mCurrNameInterface = null;
        }
    }
}
