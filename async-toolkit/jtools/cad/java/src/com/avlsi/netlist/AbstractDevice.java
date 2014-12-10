/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist;

import com.avlsi.netlist.Visitor;

/**
   Interface to a device.
 */
public interface AbstractDevice {
    /**
       Calls the appropriate method in the specified implementation of the
       Visitor interface.
       @param visitor An implementation of Visitor.
     */
    void accept(final Visitor visitor);
}
