// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public interface NodeCanonicalizer {
    /**
     * Given a node name, returns the canonical node name.
     * (Or, more generally, you can think of this as a filter which
     * transforms node names.)
     */
    NodeName canonicalize(NodeName n);
}
