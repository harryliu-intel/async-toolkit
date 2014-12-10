/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import com.avlsi.tools.presto.complete.Node;

public class CompletionInfo {
    public final Node[][] ops;
    public final int desiredSense;
    public final boolean reset;
    public final String rootName;
    public Node root;

    public CompletionInfo(Node[][] ops, int desiredSense, boolean reset,
                          String rootName) {
        this.ops = ops;
        this.desiredSense = desiredSense;
        this.reset = reset;
        this.rootName = rootName;
    }
}
