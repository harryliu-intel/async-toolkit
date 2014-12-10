/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
*/

package com.avlsi.tools.presto.complete;

public class TreeMunger {
    public static Node mungeTree(Node root, NodeBuilder builder) {
        int type = root.getType();
        if (type == Node.TERMINAL)
            return root;

        Node[] children = new Node[root.size()];
        for (int i = 0; i < root.size(); i++)
            children[i] = mungeTree(root.get(i), builder);

        return builder.buildNode(type, root.getTier(), children);
    }
}
