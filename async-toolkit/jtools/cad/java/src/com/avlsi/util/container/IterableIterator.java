package com.avlsi.util.container;

import java.util.Iterator;

public class IterableIterator<T> implements Iterable<T> {
    private final Iterator<T> iterator;
    public IterableIterator(final Iterator<T> iterator) {
        this.iterator = iterator;
    }
    public Iterator<T> iterator() {
        return iterator;
    }
}
