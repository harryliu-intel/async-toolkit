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
