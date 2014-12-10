/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Iterate over a 2D array.
 **/
public class Array2DIterator<T> implements Iterator<T> {
    private final T[][] arrays;
    private int index1;
    private int index2;

    public Array2DIterator(final T[]... arrays) {
        this.arrays = arrays;
        this.index1 = 0;
        this.index2 = 0;
    }

    public boolean hasNext() {
        // compare against 0 in case int indicies overflow
        while (0 <= index1 && index1 < arrays.length) {
            if (0 <= index2 && index2 < arrays[index1].length) {
                return true;
            } else {
                index1++;
                index2 = 0;
            }
        }

        return false;
    }

    public T next() {
        if (hasNext()) {
            return arrays[index1][index2++];
        } else {
            throw new NoSuchElementException();
        }
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}
