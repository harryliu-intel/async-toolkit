/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Turns an <code>Iterator&lt;Iterator&lt;T&gt;&gt;</code> into an
 * <code>Iterator&lt;T&gt;</code> by flattening it.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class FlatteningIterator<T> implements Iterator<T> {
    /**
     * Iterator to be flattened.
     **/
    private final Iterator<Iterator<T>> i;

    private Iterator<T> currentIterator;

    /**
     * The last iterator for which next() successfully returned an object.
     * Stored so remove() can remove the right object.
     **/
    private Iterator<T> lastNextedIterator;

    public FlatteningIterator
        (final /*@ non_null @*/ Iterator<Iterator<T>> i) {
        this.i = i;
        this.currentIterator = this.i.hasNext() ? this.i.next() : null;
        this.lastNextedIterator = null;
    }

    public FlatteningIterator(final Iterator<T>... i) {
        this(Arrays.asList(i).iterator());
    }

    public boolean hasNext() {
        if (currentIterator == null)
            return false;
        else {
            while (!currentIterator.hasNext()) {
                if (i.hasNext())
                    currentIterator = i.next();
                else {
                    currentIterator = null;
                    return false;
                }
            }
            return true;
        }
    }

    public T next() {
        if (hasNext()) {
            lastNextedIterator = currentIterator;
            return currentIterator.next();
        } else
            throw new NoSuchElementException();
    }

    public void remove() {
        if (lastNextedIterator == null)
            throw new IllegalStateException();
        else
            lastNextedIterator.remove();
    }
}
