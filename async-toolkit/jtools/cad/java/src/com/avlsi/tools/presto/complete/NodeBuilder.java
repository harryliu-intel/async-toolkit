/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
*/

package com.avlsi.tools.presto.complete;

public interface NodeBuilder {
    Node buildNode(int type, int tier, Node[] children);
}
