// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.lang.String;
import java.util.Set;
import java.util.List;

import com.avlsi.file.common.HierName;

/**
 * Get's calls forwarded from an LVSNodeCDLFactory
 **/
public interface LVSNodesHandler {

    void startCell( final String cellName );

    void lvsNodesForInstance( final HierName instnaceName,
                              final List lvsNodeConnectionPairs );

    void endCell( final String cellName, final Set cellLVSNodes );
    void abortCell( final String cellName );
}
