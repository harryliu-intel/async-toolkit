/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/file/cdl/util/rename/CDLNameInterface.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
 */

package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLRenameException;

/**
 * A transparent proxy that forwards calls to the inner CDLNameInterface.
 **/
public class CDLNameInterfaceProxy implements CDLNameInterface {

    protected CDLNameInterface inner;

    public CDLNameInterfaceProxy(final CDLNameInterface inner) {
        this.inner = inner;
    }

    public String renameCell( final String oldCellName ) 
        throws CDLRenameException {
        return inner.renameCell( oldCellName );
    }

    public String renameNode( final String oldNodeName )
        throws CDLRenameException {
        return inner.renameNode( oldNodeName );
    }

    public String renameDevice( final String oldDeviceName )
        throws CDLRenameException {
        return inner.renameDevice( oldDeviceName );
    }

    public String renameSubCellInstance( final String oldInstanceName )
        throws CDLRenameException {
        return inner.renameSubCellInstance( oldInstanceName );
    }

    public String renameTransistorModel( final String oldTransistorModel )
        throws CDLRenameException {
        return inner.renameTransistorModel( oldTransistorModel );
    }
}
