/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast.ports;

/**
 * Class representing node types.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class NodeType implements PortTypeInterface {
    private final int width;
    private final boolean arrayed;

    public NodeType() {
        this.width = 1;
        this.arrayed = false;
    }

    public NodeType(final int width) {
        this.width = width;
        this.arrayed = true;
    }

    /**
     * Returns the channel's width, ie number of <code>typeName</code>'s
     * comprising the channel.
     *
     * @return channel's width
     **/
    public int getWidth() {
        return width;
    }

    public boolean isArrayed() {
        return arrayed;
    }

    public String toString() {
        return "node of width " + width;
    }
}
