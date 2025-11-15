// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class IdentityNameInterface implements CDLNameInterface {

    public String renameCell( final String oldCellName )
        throws CDLRenameException {
        return oldCellName ;
    }

    public String renameNode( final String oldNodeName )
        throws CDLRenameException {
        return oldNodeName ;
    }

    public String renameDevice( final String oldDeviceName )
        throws CDLRenameException {
        return oldDeviceName ;
    }

    public String renameSubCellInstance( final String oldInstanceName )
        throws CDLRenameException {
        return oldInstanceName ;
    }

    public String renameTransistorModel( final String oldTransistorModel )
        throws CDLRenameException {
        return oldTransistorModel ;
    }


}
