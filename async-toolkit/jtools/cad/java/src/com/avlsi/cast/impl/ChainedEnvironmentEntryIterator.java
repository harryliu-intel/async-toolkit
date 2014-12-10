/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.cast.impl;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import com.avlsi.util.container.FlatteningIterator;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.functions.UnaryPredicate;

/**
   An iterator that will iterate over one environment and then another.
 */
public class ChainedEnvironmentEntryIterator implements EnvironmentEntryIterator {
    /**
     * Makes an EnvironmentEntryIterator an Iterator.
     **/
    private static class IteratorAdapter implements Iterator {
        private final EnvironmentEntryIterator eei;
        public IteratorAdapter(final EnvironmentEntryIterator eei) {
            this.eei = eei;
        }
        public boolean hasNext() {
            return eei.hasNext();
        }
        public Object next() {
            return eei.next();
        }
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Only iterate over EnvironmentEntries that haven't been seen before.
     **/
    private Iterator uniqueIterator(final EnvironmentEntryIterator iter) {
        return new FilteringIterator(
            new IteratorAdapter(iter),
            new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    final EnvironmentEntry ee = (EnvironmentEntry) o;
                    return seen.add(ee.getName());
                }});
    }

    /**
     * The underlying iterator.
     **/
    private final Iterator iter;

    /**
     * Contains symbols already seen
     **/
    private final Set seen;

    /**
       @param first The first environment to iterate over.
       @param second The second environment to iterate over.
     */
    public ChainedEnvironmentEntryIterator( Environment first,
                                            Environment second ){
        this( first.entryIterator(), second.entryIterator() );
    }

    /**
       @param first An iterator into the first environment to iterate over.
       @param second An iterator into the second environment to iterate over.
     */
    public ChainedEnvironmentEntryIterator( EnvironmentEntryIterator first,
                                            EnvironmentEntryIterator second ) {
        seen = new HashSet();
        iter =
            new FlatteningIterator(
                Arrays.asList(
                    new Object[] {
                        uniqueIterator(first),
                        uniqueIterator(second) }).iterator());
    }

    public final boolean hasNext() {
        return iter.hasNext();
    }

    public final EnvironmentEntry next() {
        return (EnvironmentEntry) iter.next();
    }
}
