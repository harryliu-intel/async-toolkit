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

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.util.functions.UnaryFunction;

/**
 * A map from keys to sets of values.  The implementation is not synchronized.
 *
 * @author Harry Liu
 * @version $Name:  $ $Date$
 **/
public class MultiMap<K,V> {

    /**
     * Factory returning HashSets.
     **/
    public static final CollectionFactory HASH_SET_FACTORY =
        new HashSetFactory();

    public static final <T> CollectionFactory<T> hashSetFactory() {
        return (CollectionFactory<T>) HASH_SET_FACTORY;
    }

    /**
     * Factory returning ArrayLists.
     **/
    public static final CollectionFactory ARRAY_LIST_FACTORY =
        new ArrayListFactory();

    public static final <T> CollectionFactory<T> arrayListFactory() {
        return (CollectionFactory<T>) ARRAY_LIST_FACTORY;
    }

    /**
     * A Map from keys to Collections of values.
     **/
    private final Map<K,Collection<V>> map;

    /**
     * Factory to create sets.
     **/
    private final CollectionFactory<V> collectionFactory;

    /**
     * An empty MultiMap, similar in purpose to Collections.EMPTY_MAP.
     **/
    public static final MultiMap EMPTY_MULTIMAP =
        new MultiMap(Collections.EMPTY_MAP);

    /**
     * Constructs an empty MultiMap.
     **/
    public MultiMap() {
        this(new HashMap<K,Collection<V>>());
    }

    public MultiMap(final /*@ non_null @*/ Map<K,Collection<V>> map) {
        this(map, MultiMap.<V> hashSetFactory());
    }

    public MultiMap(
            final /*@ non_null @*/ Map<K,Collection<V>> map,
            final /*@ non_null @*/ CollectionFactory<V> collectionFactory) {
        this.map = map;
        this.collectionFactory = collectionFactory;
    }

    /**
     * Adds value to the set of objects associated with <code>key</code>,
     * starting a new set if one does not already exist.  If a
     * value <code>v0</code> such that <code>v0.equals(value)</code>
     * exists in the set for <code>key</code>, then <code>value</code>
     * will not be added to the set.
     **/
    public synchronized void put(K key, V value) {
        Collection<V> collection = map.get(key);
        if (collection == null) {
            collection = collectionFactory.newCollection();
            map.put(key, collection);
        }
        collection.add(value);
    }

    /**
     * Removes the set for <code>key</code>, returning it, or
     * <code>null</code> if there was no set for <code>key</code>.
     **/
    public Collection<V> remove(Object key) {
        return map.remove(key);
    }

    /**
     * Removes <code>value</code> from <code>key</code>'s set.  If 
     * this makes <code>key</code>'s set empty, remove the set and
     * return <code>true</code>.
     * Thus, no empty sets will ever exits.  Returns <code>true</code>
     * if <code>value</code> was removed and there are no values 
     * remaining in the <code>key</code>'s set, and
     * <code>false</code> otherwise.
     **/
    public synchronized boolean remove(Object key, Object value) {
        final Collection<V> c = map.get(key);
        if (c != null) {
            c.remove(value);
            if (c.size() == 0) {
                map.remove(key);
                return true;
            }
        }
        return false;
    }

    /**
     * Returns <code>key</code>'s set, or <code>null</code> if 
     * key has no set.
     **/
    public Collection<V> get(Object key) {
        return map.get(key);
    }

    /**
     * Returns <code>true</code> if key has a set associated with it.
     **/
    public boolean containsKey(Object key) {
        return map.containsKey(key);
    }

    /**
     * Returns the set of keys.
     **/
    public Set<K> keySet() {
        return map.keySet();
    }

    public void clear() {
        map.clear();
    }
    
    /**
     * Returns a Collection view of the values contained in this MultiMap.
     **/
    public /*@ non_null @*/ Collection<V> values() {
        return new ValueCollection();
    }

    public String toString() {
        return map.toString();
    }

    /**
     * Factory for creating Sets.
     **/
    public interface CollectionFactory<V> {
        /**
         * Returns a new set.
         **/
        Collection<V> newCollection();
    }

    /**
     * Factory for creating synchronized HashSets.
     **/
    private static final class HashSetFactory implements CollectionFactory {
        public Collection newCollection() {
            return new HashSet();
        }
    }

    /**
     * Factory for creating synchronized ArrayLists.
     **/
    private static final class ArrayListFactory implements CollectionFactory {
        public Collection newCollection() {
            return new ArrayList();
        }
    }

    private final class ValueCollection extends AbstractCollection<V> {
        public Iterator<V> iterator() {
            return new FlatteningIterator<V>
                (new MappingIterator<Collection<V>,Iterator<V>>
                     (map.values().iterator(),
                      new UnaryFunction<Collection<V>,Iterator<V>>() {
                          public Iterator<V> execute(Collection<V> c) {
                              return c.iterator();
                          }
                      }));
        }

        public int size() {
            int s = 0;
            // XXX: does not handle overflow correctly, but who cares?
            for (Collection<V> c : map.values()) s += c.size();
            return s;
        }
    }
}
