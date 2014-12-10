/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist;

import java.util.NoSuchElementException;

import com.avlsi.netlist.AbstractDevice;

/**
   Interface to a device iterator, for type safety.
 */

public interface AbstractDeviceIterator {
    /**
       Returns true if the iteration has more elements.
       @return true if iterator has more elements
     */
    boolean hasNext();

    /**
       Returns the next AbstractDevice in the iteration.
       @return the next AbstractDevice
       @throws NoSuchElementException iteration has no more elements
     */
    AbstractDevice next();
}
