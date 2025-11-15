// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist;

import java.util.Iterator;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDeviceIterator;

/**
   Interface to a node.  A node represents an electrical net.  It has a
   canonical name, and aliases.
 */

public interface AbstractNode {
    /**
       Gets the canonical name of the node
       @return Name of the node
     */
    HierName getCanonicalName();

    /**
       Gets an iterator that iterates over aliases of the node
       @return An iterator containing HierNames
     */
    Iterator getAliases();

    /**
       Gets an iterator that iterates over devices connected to this node
       @return An iteration over connected devices.
     */
    AbstractDeviceIterator getDevices();
}
