// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.cosim;

/**
 * Factory to create input and output nodes.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface NodeLinkageInterface {

    /**
     * Creates an input node.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires name != null;
     * </jml></pre>
     **/
    void makeInputNode(String name);

    /**
     * Creates an output channel.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires name != null;
     * </jml></pre>
     **/
    void makeOutputNode(String name);
}
