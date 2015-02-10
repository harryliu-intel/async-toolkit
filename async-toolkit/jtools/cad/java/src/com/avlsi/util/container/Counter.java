/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Collection;
import java.util.Iterator;

public abstract class Counter<E> {
    public abstract void add(E e, int count);
    public abstract int getCount(Object e);
    public abstract Collection<E> elements();

    public int size() {
        return elements().size();
    }

    public void add(E e) {
        add(e, 1);
    }
     
    public void addAll(Counter<? extends E> counter) {
        for (E e : counter.elements()) {
            add(e, counter.getCount(e));
        }
    }

    public void subtract(E e) {
        subtract(e, 1);
    }
    
    public void subtract(E e, int count) {
        add(e, -count);
    }
    
    public void subtractAll(Counter<? extends E> counter) {
        for (E e : counter.elements()) {
            subtract(e, counter.getCount(e));
        }
    }
  
    public int getTotalCount() {
        int count = 0;
        for (E e : elements()) {
            count += getCount(e);
        }
        return count;
    }

}
