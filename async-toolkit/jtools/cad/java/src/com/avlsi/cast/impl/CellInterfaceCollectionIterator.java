/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import java.util.NoSuchElementException;

import com.avlsi.cast.CastSemanticException;
import com.avlsi.cell.CellInterface;


/**
   Iterator that iterates over collections of CellInterfaces.
   @see com.avlsi.cell.CellInterface
 */
public interface CellInterfaceCollectionIterator {

    /**
       Determines if there is another CellInterface in the collection being iterated.
       @return true if there is another CellInterface in the collection being iterated.
     */
    boolean hasNext();

    /**
       Gets the next CellInterface in the collection being iterated.
       @return The next CellInterface in the collection being iterated.

       @throws NoSuchElementException
     */
    CellInterface next() throws CastSemanticException;

}
