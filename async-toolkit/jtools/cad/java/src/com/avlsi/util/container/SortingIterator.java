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

public class SortingIterator implements Iterator {

    private final Iterator i;

    /**
     * @param i  The iterator whose next method should be transformed
     * @param p  The comparator to sort with
     **/
    public SortingIterator(final Iterator i, final Comparator c) {
        final List list = new ArrayList();
        for (; i.hasNext(); ) {
            list.add(i.next());
        }
        final Object[] array = list.toArray();
        Arrays.sort(array,c);
        this.i = Arrays.asList(array).iterator();
    }

    public SortingIterator(final Iterator i) {
        this(i,new NaturalOrderComparator());
    }

    public boolean hasNext() {
        return i.hasNext();
    }

    public Object next() {
        return i.next();
    }

    public void remove() {
        i.remove();
    }
    
}
