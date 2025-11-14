// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.layout.gdsII;


import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;

public interface TableEmitterFactoryInterface {
    TableEmitterInterface getTableEmitter( final String castCellName,
                                           final String cadenceCellName,
                                           final String gdsIICellName )
        throws TableEmitterException ;
}
