// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLRenameException;

public interface CDLNameInterface {

    String renameCell( final String oldCellName ) 
        throws CDLRenameException;

    String renameNode( final String oldNodeName )
        throws CDLRenameException;

    String renameDevice( final String oldDeviceName )
        throws CDLRenameException;

    String renameSubCellInstance( final String oldInstanceName )
        throws CDLRenameException;

    String renameTransistorModel( final String oldTransistorModel )
        throws CDLRenameException;

}
