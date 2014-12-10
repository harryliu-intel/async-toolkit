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

import java.util.Iterator;
import java.util.NoSuchElementException;

import com.avlsi.util.functions.UnaryFunction;

/**
 * This class is used to modify the data returned from an Iterator's next
 * method.
 *
 * @see Iterator
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class MappingIterator<A,Z> implements Iterator<Z> {
    private final Iterator<A> i;
    private final UnaryFunction<A,Z> f;

    /**
     * @param i  The iterator whose next method should be transformed
     * @param f  The function to carry out the transformation on next's
     *          return value.
     **/
    public MappingIterator(final Iterator<A> i, final UnaryFunction<A,Z> f) {
        this.i = i;
        this.f = f;
    }

    public boolean hasNext() {
        return i.hasNext();
    }
        
    /**
     * Applies <code>f.execute</code> to the underlying iterator's next
     * element.
     **/
    public Z next() {
        return f.execute(i.next());
    }

    public void remove() {
        i.remove();
    }
}
