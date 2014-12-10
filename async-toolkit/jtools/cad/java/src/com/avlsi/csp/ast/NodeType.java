/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * Class for CSP node type
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class NodeType extends Type {
    /**
     * Number of bits of information the possibly wide node can carry.
     **/
    private final int width;

    /**
     * The direction of the node.
     **/
    private final PortDirection direction;

    /**
     * Whether this is a "wide" node.  Used to distinguish node from node[1].
     **/
    private final boolean arrayed;

    public NodeType(final int width, final PortDirection direction,
                    final boolean arrayed) {
        assert width > 0 : "Illegal node width: " + width;
        assert width == 1 || arrayed : "Wide node must be arrayed: " + width;
        this.direction = direction;
        this.width = width;
        this.arrayed = arrayed;
    }

    public NodeType(final PortDirection direction) {
        this(1, direction, false);
    }

    public void accept(VisitorInterface v) throws VisitorException {
        v.visitNodeType(this);
    }

    public int getWidth() {
        return width;
    }

    public int dimension() {
        return 0;
    }

    public boolean isArrayed() {
        return arrayed;
    }

    public String toString() {
        return arrayed ? "node[" + width + "]" : "node";
    }
}
