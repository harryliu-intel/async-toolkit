package com.avlsi.util.container;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class ReverseArray<T> implements Iterable<T> {
    private final T[] array;
    public ReverseArray(final T[] array) {
        this.array = array;
    }
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            int index = array.length - 1;
            public boolean hasNext() {
                return index >= 0;
            }
            public T next() {
                if (hasNext()) {
                    return array[index--];
                } else {
                    throw new NoSuchElementException();
                }
            }
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }
}
