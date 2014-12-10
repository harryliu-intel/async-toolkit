/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryFunction;

/**
 * A map from a set of key <code>Objects</code> to a data
 * </code>Object.  Keys in the same set are said to be equivalent.
 * Operations supported are:
 * <ul>
 *   <li> Objects may be equivalenced together by
 *   <code>makeEquivalent(Object key1, Object key2)</code>,
 *   <li> A new set may be started with <code>addData(Object key,
 *   Object value)</code>,
 *   <li> Equivalent keys may be retrieved with
 *   <code>getAliases()</code>, and
 *   <li> The stored value may be retrieved with <code>getValue()</code>.
 * </ul>
 * The key objects must implement the <code>Comparable</code> interface.
 *
 * @design jmr maybe split out Alias into Alias and AliasedValue,
 * which would avoid all the null pointers in Aliases
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 * @review kiniry 18-23 July 2002
 *
 * @bug kiniry 23 July 2002 - MultiSet is being abused (memory- and
 * performance-wise) probably via this class.  The primary culprits
 * are find(), findIndex(), and binarySearch().  E.g., approximately
 * 25% of the runtime of moderate sized PrsToNet jobs is spent in this
 * class.
 * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1168">Bug#1168</a>
 **/
public final class AliasedMap {
    /**
     * A set of the Aliases.
     **/
    private final MultiSet aliasSet;

    /**
     * The function to use to merge contents when two previously
     * unaliased keys are aliased together. 
     **/
    private final MergeFunction mergeFunction;

    /**
     * Comparator to be used in getCanonicalKey
     **/
    private final Comparator canonicalnessComparator;

    /**
     * Exception thrown by AliasedMap.XXX and AliasedData.addData if
     * aliased data cannot be merged.
     **/
    public static class MergeFailedException extends Exception {
        public MergeFailedException() {
            super();
        }

        public MergeFailedException(final String message) {
            super(message);
        }
    }

    /**
     * Interface to merge data in AliasedMap.
     * This is passed to the AliasedMap constructor, and 
     * used in AliasedMap.XXX, and AliasedData.XXX.
     **/
    public interface MergeFunction {
        Object merge(Object oldValue, Object newValue)
            throws MergeFailedException;
    }


    /**
     * Class constructor.  Use the natural order to determine canonicalness.
     * @param mergeFunction Used by merge() to merge the data for
     *   two alias sets.
     **/
    public AliasedMap(final MergeFunction mergeFunction) {
        this(mergeFunction, new NaturalOrderComparator());
    }

    /**
     * Class constructor.  
     * @param mergeFunction Used by merge() to merge the data for
     *   two alias sets.
     * @param comparator comparison function used 
     **/
    public AliasedMap(final MergeFunction mergeFunction,
                      final Comparator canonicalnessComparator) {
        this(mergeFunction, canonicalnessComparator, canonicalnessComparator);
    }

    /**
     * Class constructor.  
     * @param mergeFunction Used by merge() to merge the data for
     *   two alias sets.
     * @param canonicalnessComparator comparison function used to determine
     * ordering
     * @param existenceComparator comparison function used to determine
     * existence
     **/
    public AliasedMap(final MergeFunction mergeFunction,
                      final Comparator canonicalnessComparator,
                      final Comparator existenceComparator) {
        this.mergeFunction = mergeFunction;
        this.canonicalnessComparator = canonicalnessComparator;
        this.aliasSet
            = new MultiSet(new Alias.AliasComparator(existenceComparator));
    }

    /**
     * Returns the Alias associated with key, or a new Alias with key
     * <code>key</code> and value <code>value</code> if it is not known.
     * Adds that new alias to the <code>aliasSet</code>.
     **/
    private Alias getOrAddAlias(final Object key,
                                final Object value) {
        final Alias aNew = new Alias(key, value);
        final Alias a = (Alias) aliasSet.find(aNew);

        if (a == null) {
            aliasSet.add(aNew);
            return aNew;
        } else
            return a;
    }

    /**
     * Returns the Alias associated with key, or a new Alias with key
     * <code>key</code> and value <code>null</code> if it is not known.
     * Adds that new key to the <code>aliasSet</code>.
     **/
    private Alias getOrAddAlias(final Object key) {
        return getOrAddAlias(key, null);
    }

    /**
     * Returns the Alias associated with key, or a <code>null</code>
     * if it is not known.  Does not add that new key to the
     * <code>aliasSet</code>.
     **/
    private Alias findAlias(final Object key) {
        final Alias aTemp = new Alias(key, null);
        return (Alias) aliasSet.find(aTemp);
    }

    /**
     * Makes key1 and key2 equivalent.
     * <ul>
     *   <li> If neither key1 nor key2 is in the map, start a new set,
     *   <li> If one of key1 and key2 is in the map, add the one
     *        that is not in the map 
     *   <li> If both key1 and key2 are in the map and they are not
     *        already equivalent, union their key sets, and merge their
     *        values with mergeFunction.
     *   <li> If both key1 and key2 are in the map and they are 
     *        already equivalent, do nothing.
     * </ul>
     **/
    public void makeEquivalent(final Object key1, final Object key2)
        throws MergeFailedException
    {
        Debug.assertNotNull(key1);
        Debug.assertNotNull(key2);

        final Alias a1 = getOrAddAlias(key1);
        final Alias a2 = getOrAddAlias(key2);

        a1.makeEquivalent(a2, mergeFunction, canonicalnessComparator);
    }

    /**
     * Returns whether the two keys are equivalent.  Returns false
     * if either key is not known.
     **/
    public boolean areEquivalent(final Object key1, final Object key2) {
        final Alias a1 = findAlias(key1);

        if (a1 == null)
            return false;
        else {
            final Alias a2 = findAlias(key2);

            if (a2 == null)
                return false;
            else
                return a1.isEquivalentTo(a2);
        }
    }

    /**
     * <ul>
     *   <li> If key is in the map, merge the new data with 
     *        the existing data.
     *   <li> If key is not in the map, start a new set with the given
     *        value.
     * </ul>
     **/
    public void addData(final Object key, final Object value)
        throws MergeFailedException
    {
        Debug.assertNotNull(key);

        final Alias a = findAlias(key);

        if (a == null) {
            final Alias aNew = new Alias(key, value);
            aliasSet.add(aNew);
        } else {
            a.mergeValue(value, mergeFunction);
        }
    }

    /**
     * If key is in map, returns an unmodifiable Iterator of all the
     * aliases for a key, if key is not in map, returns null.
     * @todo jmr Should be renamed to getEquivalentKeys
     **/
    public Iterator getAliases(final Object key) {
        Debug.assertNotNull(key);

        final Alias a = findAlias(key);

        if (a == null)
            return null;
        else
            return a.getAliasedKeys();
    }

    /**
     * returns an unmodifiable iterator over all the keys
     **/
    // untested
    public Iterator getKeys() {
        return Alias.getKeys(aliasSet.iterator());
    }

    /**
     * returns an unmodifiable Iterator of all the values.  (One value 
     * per set of keys, so no value is repeated.)
     **/
    // untested
    public Iterator getValues() {
        return Alias.getValues(getCanonicalAliases());
    }

    /**
     * Returns the value associated with key.
     **/
    public Object getValue(final Object key) {
        Debug.assertNotNull(key);

        final Alias a = findAlias(key);

        if (a == null)
            return null;
        else
            return a.getRootValue();
    }

    /**
     * Set the value associated with the key, returning the old value.
     * If the key is not present, add it, and return null.  The
     * MergeFunction will not be called, the value will just be replaced.
     **/
    public Object setValue(final Object key, final Object value) {
        Debug.assertNotNull(key);

        final Alias a = findAlias(key);

        if (a == null) {
            final Alias aNew = new Alias(key, value);
            aliasSet.add(aNew);
            return null;
        } else {
            return a.setRootValue(value);
        }
    }

    /**
     * If both keys are in the map, tells whether or not they are
     * equivalent, otherwise, returns false.
     **/
    // untested
    public boolean equivalent(final Object k1, final Object k2) {
        Debug.assertNotNull(k1);
        Debug.assertNotNull(k2);

        final Alias a1 = findAlias(k1);
        final Alias a2 = findAlias(k2);

        if (a1 == null)
            return false;
        else if (a2 == null)
            return false;
        else
            return a1.isEquivalentTo(a2);
    }

    public boolean contains(final Object key) {
        return findAlias(key) != null;
    }

    /**
     * Gets the canonical key for key <code>k</code>.
     * If k is not known, return null.
     **/
    public Object getCanonicalKey(final Object k) {
        final Alias a = findAlias(k);

        if (a == null)
            return null;
        else
            return a.getRootAlias().getKey();
    }

    /**
     * Returns an unmodifiable Iterator of Objects of all canonical keys.
     **/
    public Iterator getCanonicalKeys() {
        return Alias.getKeys(getCanonicalAliases());
    }

    /**
     * Returns an unmodifiable Iterator of Aliases that are canonical keys.
     **/
    private Iterator getCanonicalAliases() {
        // make a new multi set to hold the canonical aliases
        final MultiSet canonicalAliases
            = new MultiSet(new Alias.AliasComparator(canonicalnessComparator));

        // go through all aliases, adding the canonical alias if it is not 
        // there
        for (final Iterator i = aliasSet.iterator(); i.hasNext(); ) {
            final Alias a = (Alias) i.next();
            canonicalAliases.addIfUnique(a.getRootAlias());
        }

        return canonicalAliases.iterator();
    }

    /**
     * Returns the merge function in use.
     **/
    public MergeFunction getMergeFunction() {
        return mergeFunction;
    }

    /**
     * Returns the comparator in use.
     **/
    public Comparator getComparator() {
        return canonicalnessComparator;
    }

    /**
     * Incorporates the map of asm into the current map, applying
     * <code>f</code> to each value as it is imported, and
     * prepending prefix to each equivalent key.
     **/
    // untested!!!
    public void adopt(final String prefix,
                      final UnaryFunction f,
                      final AliasedMap asm)
        throws MergeFailedException
    {
        throw new MergeFailedException("update me to support non-String");
        /*
        for (final Iterator iAd = asm.valueSet.iterator(); iAd.hasNext(); ) {
            final AliasedData ad = (AliasedData) iAd.next();
            final Object v = f.execute(ad.getValue());

            final Iterator iEquivNames = ad.getAliases();
            final String firstName = (String) iEquivNames.next();

            addData(prefix + firstName, v);

            while (iEquivNames.hasNext()) {
                final String n = prefix + (String) iEquivNames.next();
                makeEquivalent(firstName, n);
            }
        }
        */
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        for (final Iterator iCanon = getCanonicalKeys();
             iCanon.hasNext(); ) {

            final Object canon = iCanon.next();

            sb.append("< " + canon.toString());

            for (final Iterator iConn = getAliases(canon);
                 iConn.hasNext(); ) {

                final Object conn = iConn.next();

                sb.append(" = " + conn.toString());
            }

            sb.append(" > = ");
            sb.append(getValue(canon));
            sb.append("\n");
        }

        return sb.toString();
    }
}
