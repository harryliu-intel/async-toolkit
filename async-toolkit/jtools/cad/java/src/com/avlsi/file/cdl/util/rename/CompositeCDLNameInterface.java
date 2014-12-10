/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class CompositeCDLNameInterface implements CDLNameInterface {

    final protected CDLNameInterface f,g;

    public CompositeCDLNameInterface(final CDLNameInterface f, final CDLNameInterface g) {
        this.f = f;
        this.g = g;
    }

    public String renameCell( final String oldCellName )
        throws CDLRenameException {        
        return g.renameCell( f.renameCell(oldCellName) );
    }

    public String renameNode( final String oldNodeName )
        throws CDLRenameException {
        return g.renameNode( f.renameNode(oldNodeName) );
    }

    public String renameDevice( final String oldDeviceName )
        throws CDLRenameException {
        return g.renameDevice( f.renameDevice(oldDeviceName) );
    }

    public String renameSubCellInstance( final String oldInstanceName )
        throws CDLRenameException {
        return g.renameSubCellInstance( f.renameSubCellInstance(oldInstanceName) );
    }

    public String renameTransistorModel( final String oldTransistorModel )
        throws CDLRenameException {
        return g.renameTransistorModel( f.renameTransistorModel(oldTransistorModel) );
    }
}
