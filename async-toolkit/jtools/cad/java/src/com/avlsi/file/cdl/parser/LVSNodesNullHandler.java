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

import com.avlsi.file.cdl.parser.LVSNodesHandler;

public class LVSNodesNullHandler implements LVSNodesHandler {

    public LVSNodesNullHandler() { }

    public void startCell( final String cellName ) {}

    public void lvsNodesForInstance( final HierName instnaceName,
                                     final List lvsNodeConnectionPairs ) {}

    public void endCell( final String cellName, final Set cellLVSNodes ) {}

    public void abortCell( final String cellName ) {}

}
