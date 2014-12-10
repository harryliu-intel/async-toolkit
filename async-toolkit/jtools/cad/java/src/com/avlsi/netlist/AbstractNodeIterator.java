/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist;

import java.util.NoSuchElementException;

import com.avlsi.netlist.AbstractNode;

/**
   Interface to a node iterator, for type safety.
 */

public interface AbstractNodeIterator {
    /**
       Returns true if the iteration has more elements.
       @return true if iterator has more elements
     */
    boolean hasNext();

    /**
       Returns the next AbstractNode in the interation.
       @return the next AbstractNode
       @throws NoSuchElementException iteration has no more elements
     */
    AbstractNode next();
}
