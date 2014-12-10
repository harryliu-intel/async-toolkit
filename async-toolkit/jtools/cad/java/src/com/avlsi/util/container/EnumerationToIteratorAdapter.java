/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Adapts an Enumeration into an Iterator.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class EnumerationToIteratorAdapter implements Iterator {
    private final Enumeration e;

    public EnumerationToIteratorAdapter(final Enumeration e) {
        this.e = e;
    }

    public boolean hasNext() {
        return e.hasMoreElements();
    }

    public Object next() {
        return e.nextElement();
    }

    public void remove() throws UnsupportedOperationException {
        throw new UnsupportedOperationException();
    }
}
