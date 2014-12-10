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
