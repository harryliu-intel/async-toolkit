/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Changes an iterator into an iterator that cannot be modified.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class UnmodifiableIterator implements Iterator {
    private final Iterator i;

    public UnmodifiableIterator(final Iterator i) {
        this.i = i;
    }

    public boolean hasNext() {
        return i.hasNext();
    }

    public Object next() {
        return i.next();
    }

    /**
     * @throws UnsupportedOperationException  Always.
     **/
    public void remove() {
        throw new UnsupportedOperationException("remove not allowed " +
                "on an UnmodifiableIterator");
    }
}
