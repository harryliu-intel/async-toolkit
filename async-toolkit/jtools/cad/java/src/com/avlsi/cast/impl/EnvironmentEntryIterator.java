/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import java.util.NoSuchElementException;


/**
 * Interface to an iterator that iterates over all the values in an
 * environment.  The set of entries returned by the iterator must be exactly
 * the same set of entries that could be looked up through the Environment
 * interface.  This means that one key must never appear in more than one
 * entry.
 **/
public interface EnvironmentEntryIterator {
    /**
     * Determines if the iteration has another element.
     * @return true if there is another element in the iteration, false
     *         otherwise.
     **/
    boolean hasNext();

    /**
     * Retrieves the next element in the iteration.
     * @return The next element in the iteration.
     * @throws NoSuchElementException
     **/
    EnvironmentEntry next();
}
