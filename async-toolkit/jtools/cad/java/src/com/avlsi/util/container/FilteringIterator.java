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

import com.avlsi.util.functions.UnaryPredicate;

/**
 * This class is used to filter an iterator according to a predicate.
 *
 * @see Iterator
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class FilteringIterator<E> implements Iterator<E> {
    private final Iterator<E> i;
    private final UnaryPredicate<E> p;
    private E nextObject;

    /**
     * @param i  The iterator whose next method should be transformed
     * @param p  The predicate to use to filter the iterator.
     **/
    public FilteringIterator(final Iterator<E> i, final UnaryPredicate<E> p) {
        this.i = i;
        this.p = p;
        this.nextObject = null;
        findNext();
    }

    public boolean hasNext() {
        return nextObject != null;
    }
        
    /**
     * Applies <code>p.evalutate</code> to the underlying iterator's
     * sucessive next elements until p returns true.
     **/
    public E next() {
        if (nextObject == null)
            throw new NoSuchElementException();
        else {
            final E o = nextObject;
            findNext();
            return o;
        }
    }

    public void remove() {
        // Throw UnsupportedOperationException to avoid pain getting it
        // to work.  next(); hasNext(); remove(); is required to remove
        // the object returned by next().  Thus we must hang on to that.
        // Bleh.
        throw new UnsupportedOperationException();
    }

    /**
     * Advances iterator until an object satisfying the predicate is
     * found.  When such is found, it is stored in nextObject and returned.
     * If no satisfying object could be found, null is stored in
     * nextObject and returned.
     **/
    private void findNext() {
        while (i.hasNext()) {
            nextObject = i.next();

            if (p.evaluate(nextObject))
                return;
        }

        nextObject = null;
    }

}
