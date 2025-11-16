// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
*/

package com.avlsi.tools.presto.complete;

public interface NodeBuilder {
    Node buildNode(int type, int tier, Node[] children);
}
