/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.avlsi.util.functions.UnaryFunction;

/**
 * Helper class for {@link AliasedMap}.  A set of <code>Alias</code>es 
 * is used to associate a set of keys with one value.  The set of keys
 * is said to be "connected" or "equivalent".  One of these keys is
 * the "canonical key" or "root alias".  The "canonical key" is the 
 * key with the least value according to the <code>canonicalnessComp</code>
 * passed to {@link #makeEquivalent}.
 *
 * @see AliasedMap
 *
 * @author Andrew Lines
 * @author Jesse Rosenstock
 * @version $Date$
 * @review kiniry 16-23 July 2002
 **/
public class Alias {
    /**
     * The key.
     **/
    private final Object key;

    /**
     * The value, will be null unless the alias is a root alias.
     **/
    private Object value;

    /**
     * A pointer that points towards the root alias.  Following the root
     * pointers gives a null terminated list ending at the root alias.
     * The root pointer tree is compacted whenever it is accessed.
     **/
    private Alias root;

    /**
     * A pointer to the next equivalent Alais.  The next pointers form
     * a circularly linked list.
     **/
    private Alias next;

    /**
     * Turns a comparator of keys into a comparator of aliases.
     **/
    public static final class AliasComparator implements Comparator {
        /**
         * the key comparator
         **/
        private final Comparator keyComparator;

        /**
         * Class constructor
         **/
        public AliasComparator(final Comparator keyComparator) {
            this.keyComparator = keyComparator;
        }

        /**
         * Compares two aliases using the comparator for their keys.
         *
         * @bug kiniry 23 July 2002 - 
         **/
        public int compare(final Object o1, final Object o2) {
            final Alias a1 = (Alias) o1;
            final Alias a2 = (Alias) o2;

            return keyComparator.compare(a1.getKey(), a2.getKey());
        }

        /**
         * Returns whether this AliasComparator is equal to some object;
         * true if the other object is an AliasComparator and their
         * keyComparators are equal.
         **/
        public boolean equals(final Object o) {
            if (o instanceof AliasComparator)
                return equals((AliasComparator) o);
            else
                return false;
        }

        /**
         * Returns whether this AliasComparator is equal to another;
         * true if their keyComparators are equal.
         **/
        public boolean equals(AliasComparator ac) {
            return keyComparator.equals(ac.keyComparator);
        }
    }

    /**
     * Construct an alias with specified key and value.
     * 
     * @param key  Non-null key
     * @param value  value object may be null
     *
     * @exception  IllegalArgumentException if key is null
     **/
    public Alias(final Object key, final Object value) {
        if (key == null)
            throw new IllegalArgumentException("null key");

        this.key = key;
        this.value = value;
        root = null;
        next = this;
    }

    /**
     * @return the key.
     *
     * @bug kiniry 23 July 2002 - Key method for performance evaluation as a
     * moderate run of PrsToNet calls getKey() >2.5 times and accounts for
     * nearly 1% of run-time.  The only potential optimization I can see is
     * making this method final so that it can be inlined better.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1171">Bug#1171</a>
     **/
    public Object getKey() {
        return key;
    }

    /**
     * Return the value.
     **/
    public Object getValue() {
        return value;
    }

    /**
     * Set the value, returning the old one.
     **/
    public Object setValue(final Object newValue) {
        final Object oldValue = value;
        value = newValue;
        return oldValue;
    }

    /**
     * Return the value stored at this alias's root.
     **/
    public Object getRootValue() {
        return getRootAlias().getValue();
    }

    /**
     * Set the value stored at the root alias, returning the old one.
     **/
    public Object setRootValue(final Object newValue) {
        return getRootAlias().setValue(newValue);
    }

    /**
     * Return the root Alias (and compact the pointer tree).
     **/
    public Alias getRootAlias() {
        if (root == null)
            return this;

        root = root.getRootAlias();
        return root;
    }

    /** 
     * Make equivalent (connect) to another Alias.  Their values will be
     * merged with <code>mergeFunction</code>.  The circular lists of
     * next pointers will be spliced together, and the appropriate
     * root pointer updated.
     *
     * @param that   the alias to connect to
     * @param mergeFunction  the function to use to merge the values of
     *   the aliases
     * @param canonicalnessComp  comparator for the keys determining
     *   which is more canonical
     *
     * @exception MergeFunction if the values could not be merged
     **/
    public void makeEquivalent(final Alias that,
                               final AliasedMap.MergeFunction mergeFunction,
                               final Comparator canonicalnessComp)
        throws AliasedMap.MergeFailedException {

        Alias aRoot = this.getRootAlias();
        Alias bRoot = that.getRootAlias();

        if (aRoot == bRoot)
            return; // already connected

        if (canonicalnessComp.compare(aRoot.getKey(), bRoot.getKey()) > 0) {
            final Alias t = bRoot;
            bRoot = aRoot;
            aRoot = t;
        }

        // aRoot <= bRoot

        // aRoot <= bRoot, merge b's data with a's, and null out b's
        aRoot.mergeValue(bRoot.value, mergeFunction);
        bRoot.value = null;

        // aRoot <= bRoot, so set b's root to be a's
        bRoot.root = aRoot;

        // join the rings of aliases
        final Alias aRootNext = aRoot.next;
        final Alias bRootNext = bRoot.next;
        aRoot.next = bRootNext;
        bRoot.next = aRootNext;
    }

    /**
     * Merges the value of this alias's root with <code>value</code>.
     *
     * @param value  the value to merge with the alias's current value
     * @param mergeFunction  the function to use to merge the values of
     *   the aliases
     *
     * @exception MergeFunction if the values could not be merged
     **/
    public void mergeValue(final Object value,
                           final AliasedMap.MergeFunction mergeFunction)
        throws AliasedMap.MergeFailedException {
        getRootAlias().value
            = mergeFunction.merge(getRootAlias().getValue(), value);
    }

    /**
     * Check if connected to a specified Alias.
     * that must not be null
     **/
    public boolean isEquivalentTo(Alias that) {
        return this.getRootAlias() == that.getRootAlias();
    }  

    /**
     * Return iterator over all Aliases connected to this one,
     * including this alias itself.
     **/
    public Iterator getEquivalentAliases() {
        return new Iterator() {
            private Alias current = Alias.this;
            private final Alias first = Alias.this;
            private boolean done = false;

            public boolean hasNext() {
                return !done;
            }

            public Object next() {
                if (done)
                    throw new NoSuchElementException();

                final Alias a = current;
                current = current.next;
                done = (current==first);
                return a;
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    /**
     * Returns an iterator over the keys of the equivalent aliases.
     *
     * @return  an Iterator of key Objects
     **/
    public Iterator getAliasedKeys() {
        return getKeys(getEquivalentAliases());
    }

    /**
     * Maps an Iterator of Aliases to an Iterator of keys.
     *
     * @return  an Iterator of key Objects
     **/
    public static Iterator getKeys(final Iterator iAlias) {
        return new MappingIterator(iAlias, new UnaryFunction() {
            public Object execute(final Object o) {
                final Alias a = (Alias) o;
                return a.getKey();
            }
        });
    }

    /**
     * Maps an Iterator of Aliases to an Iterator of values.
     *
     * @return  an Iterator of key Objects
     **/
    public static Iterator getValues(final Iterator iAlias) {
        return new MappingIterator(iAlias, new UnaryFunction() {
            public Object execute(final Object o) {
                final Alias a = (Alias) o;
                return a.getValue();
            }
        });
    }

    /** 
     * Return string representation for debugging.
     **/
    public String toString() {
        return key + "{" + value + "}";
    }

    public int hashCode() {
        throw new RuntimeException("Don't use me, I don't know what to do");
        //return key.hashCode() ^ (value == null ? 0 : value.hashCode());
    }

    public boolean equals(final Object o) {
        throw new RuntimeException("Don't use me, I don't know what to do");
        //return this == o;
    }
}
