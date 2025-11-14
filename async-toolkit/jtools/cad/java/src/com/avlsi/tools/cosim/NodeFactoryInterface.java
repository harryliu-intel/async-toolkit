// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.cosim;

import com.avlsi.tools.tsim.WideNode;

/**
 * Factory to create nodes.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public interface NodeFactoryInterface {
    /**
     * Creates a wide node.
     **/
    WideNode makeWideNode(String name, int width, int direction,
                          boolean isArrayed, boolean readOnly);
}
