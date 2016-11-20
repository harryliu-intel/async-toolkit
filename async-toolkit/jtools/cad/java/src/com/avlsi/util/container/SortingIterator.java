/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Comparator;

public class SortingIterator<T> implements Iterator<T> {

    private final Iterator<T> i;

    /**
     * @param i  The iterator whose next method should be transformed
     * @param c  The comparator to sort with
     **/
    public SortingIterator(final Iterator<T> i, final Comparator<T> c) {
        final List<T> list = new ArrayList<>();
        for (; i.hasNext(); ) {
            list.add(i.next());
        }
        list.sort(c);
        this.i = list.iterator();
    }

    public SortingIterator(final Iterator i) {
        this(i,new NaturalOrderComparator());
    }

    public boolean hasNext() {
        return i.hasNext();
    }

    public T next() {
        return i.next();
    }

    public void remove() {
        i.remove();
    }
    
}
