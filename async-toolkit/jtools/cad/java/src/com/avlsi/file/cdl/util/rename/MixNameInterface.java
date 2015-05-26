/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/file/cdl/util/rename/GDS2NameInterface.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
 */


package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class MixNameInterface implements CDLNameInterface {
    private final CDLNameInterface cell;
    private final CDLNameInterface node;
    private final CDLNameInterface device;
    private final CDLNameInterface instance;
    private final CDLNameInterface transistor;

    public MixNameInterface(final CDLNameInterface cell,
                            final CDLNameInterface node,
                            final CDLNameInterface device,
                            final CDLNameInterface instance,
                            final CDLNameInterface transistor) {
        this.cell = cell;
        this.node = node;
        this.device = device;
        this.instance = instance;
        this.transistor = transistor;
    }

    public String renameCell(String name ) 
        throws CDLRenameException
    {
        return cell.renameCell(name);
    }

    public String renameNode( final String oldNodeName ) 
        throws CDLRenameException
    {
        return node.renameNode(oldNodeName);
    }

    public String renameDevice( final String oldDeviceName ) 
        throws CDLRenameException 
    {
        return device.renameDevice(oldDeviceName);
    }

    public String renameSubCellInstance( final String oldInstanceName ) 
        throws CDLRenameException 
    {
        return instance.renameSubCellInstance(oldInstanceName);
    }

    public String renameTransistorModel( final String oldTransistorModel )
        throws CDLRenameException 
    {
        return transistor.renameTransistorModel(oldTransistorModel);
    } 
}
