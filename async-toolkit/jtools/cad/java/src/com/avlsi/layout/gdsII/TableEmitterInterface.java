// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.layout.gdsII;


import com.avlsi.layout.gdsII.TableEmitterException;

public interface TableEmitterInterface {  

    boolean haveCellName( final String castName );
    void emitCellName( final String castName, 
                       final String cadenceName, 
                       final String gdsIIName ) throws TableEmitterException ;
    boolean haveNodeName( final String castName );
    void emitNodeName( final String castName, 
                       final String cadenceName,
                       final String gdsIIName ) throws TableEmitterException ;
    boolean haveInstanceName( final String castName );
    void emitInstanceName( final String castName,
                           final String cadenceName,
                           final String gdsIIName ) throws TableEmitterException ;

    void close() throws TableEmitterException ;
}
