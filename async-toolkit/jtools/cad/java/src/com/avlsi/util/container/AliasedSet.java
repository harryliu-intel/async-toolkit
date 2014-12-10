/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Comparator;
import java.util.Iterator;

import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

/**
 * A set of sets of objects.
 * <ul>
 *   <li> Objects may be equivalenced together by
 *       makeEquivalent(Object key1, Object key2).  
 *   <li> A new set may be started with add(Object key)
 *   <li> Equivalent keys may be retrieved with getAliases()
 * </ul>
 * The key objects must implement Comparable.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class AliasedSet {
    /**
     * The AliasedMap on which the set is based.
     **/
    private final AliasedMap aliasedMap;

    /**
     * Flag value object to indicate an object is present in the set.
     **/
    private static final Object PRESENT = new Object();

    /**
     * Merge function, always returns null.
     **/
    private static final class MyMergeFunction
            implements AliasedMap.MergeFunction {
        public Object merge(final Object v1, final Object v2) {
            Debug.assertTrue(v1 == PRESENT);
            Debug.assertTrue(v2 == PRESENT);

            return PRESENT;
        }
    }

    private static final AliasedMap.MergeFunction mergeFunction =
        new MyMergeFunction();

    /**
     * Class constructor.  Use the natural order to determine canonicalness.
     **/
    public AliasedSet() {
        aliasedMap = new AliasedMap(mergeFunction);
    }

    /**
     * Class constructor.
     **/
    public AliasedSet(final Comparator canonicalnessComparator) {
        aliasedMap = new AliasedMap(mergeFunction, canonicalnessComparator);
    }

    /**
     * Class constructor.
     **/
    public AliasedSet(final Comparator canonicalnessComparator,
                      final Comparator existenceComparator) {
        aliasedMap = new AliasedMap(mergeFunction, canonicalnessComparator,
                                    existenceComparator);
    }

    /**
     * Class constructor, treats the underlying AliasedMap as an AliasedSet.
     * This is really a horrible hack.  Perhaps interfaces should be made,
     * or something.
     **/
    public AliasedSet(final AliasedMap aliasedMap) {
        this.aliasedMap = aliasedMap;
    }

    /**
     * Makes key1 and key2 equivalent.
     * <ul>
     *   <li> If neither key1 nor key2 is in the set, start a new set.
     *   <li> If one of key1 and key2 is in the set, add the one
     *        that is not in the set
     *   <li> If both key1 and key2 are in the set and they are not
     *        already equivalent, union their key set
     *   <li> If both key1 and key2 are in the set and they are
     *        already equivalent, do nothing.
     * </ul>
     **/
    public void makeEquivalent(final Object key1, final Object key2)
    {
        try {
            add(key1);
            add(key2);
            aliasedMap.makeEquivalent(key1, key2);
        } catch (AliasedMap.MergeFailedException e) {
            // ok, this can happen if we're wrapping an external map
            throw new AssertionFailure(e);
        }
    }

    /**
     * Returns whether the two keys are equivalent.  Returns false
     * if either key is not known.
     **/
    public boolean areEquivalent(final Object key1, final Object key2) {
        return aliasedMap.areEquivalent(key1, key2);
    }

    /**
     * Adds the key to the set.
     **/
    public void add(final Object key) {
        try {
            aliasedMap.addData(key, PRESENT);
        } catch (AliasedMap.MergeFailedException e) {
            // ok, this can happen if we're wrapping an external map
            throw new AssertionFailure(e);
        }
    }

    public boolean contains(final Object key) {
        final Object o = aliasedMap.getValue(key);
        
        if (o == null)
            return false;
        else {
            // o == PRESENT won't hold if we are wrapping someone else's
            // aliasedMap
            return true;
        }
    }

    /**
     * If key is in set, returns an unmodifiable Iterator of all the
     * aliases for a key, if key is not in set, returns null.
     * @todo jmr Should be renamed to getEquivalentKeys
     **/
    public Iterator getAliases(final Object key) {
        return aliasedMap.getAliases(key);
    }

    /**
     * Gets the canonical key for key <code>k</code>.
     * If k is not known, return null.
     **/
    public Object getCanonicalKey(final Object k) {
        return aliasedMap.getCanonicalKey(k);
    }

    /**
     * Returns an AliasedMap, with the same keys, and <code>v</code> for
     * the value.
     **/
    public AliasedMap toAliasedMap(final AliasedMap.MergeFunction mf,
                                   final Object v)
        throws AliasedMap.MergeFailedException {
        final AliasedMap am = new AliasedMap(mf, aliasedMap.getComparator());

        for (final Iterator iCanon = getCanonicalKeys();
                iCanon.hasNext(); ) {
            final Object canon = iCanon.next();

            am.addData(canon, v);

            for (final Iterator iConn = getAliases(canon);
                    iConn.hasNext(); ) {
                final Object conn = iConn.next();

                am.makeEquivalent(canon, conn);
            }
        }

        return am;
    }

    /**
     * Returns an unmodifiable Iterator of Objects of all canonical keys.
     **/
    public Iterator getCanonicalKeys() {
        return aliasedMap.getCanonicalKeys();
    }

    public String toString() {
        return aliasedMap.toString();
    }

}
