/*
 *      CollectionUtils.java - stuff I wish was in java.util.Collections
 *
 *      Copyright 2002-2004 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.avlsi.util.container;

import java.util.Arrays;
import java.util.ArrayList;
import java.math.BigDecimal;
import java.io.BufferedReader;
import java.util.Collection;
import java.util.Collections;
import java.io.IOException;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.functions.BinaryPredicate;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

/**
 * Utilites operating on Collections.
 *
 * @author Patrick Pelletier
 */

public class CollectionUtils {
    /**
     * This class should not be instantiated.
     **/
    private CollectionUtils() { }

    /**
     * Converts a Collection (whose members must all be subclasses of
     * Number, such as Integer) into an int[] array.  So, this is much
     * like toArray() in Collection, except that it gives you a primitive
     * int[] array, rather than an object Integer[] array.
     * @param  c   the collection to convert
     * @return an int[] array
     */
    public static int[] toIntArray(Collection c) {
        int[] result = new int[c.size()];

        int j = 0;
        for (Iterator i = c.iterator(); i.hasNext(); j++) {
            Number n = (Number) i.next();
            result[j] = n.intValue();
        }

        return result;
    }

    /**
     * Converts an int[] array into a List of Integers.  This is much
     * like asList() in java.util.Arrays, except that it takes a
     * primitive int[] array, rather than an Object[] array.
     * @param a  the array to convert
     * @return a List containing Integers
     */
    public static List asList(int[] a) {
        Integer[] result = new Integer[a.length];

        for (int j = 0; j < a.length; j++)
            result[j] = new Integer(a[j]);

        return Arrays.asList(result);
    }

    /**
     * Given an even-length array of Objects, treats them as key-value
     * pairs and constructs a Map out of them, much the same way that
     * Perl automatically converts an array into a hash.
     *
     * This is useful for statically initializing a map, like this:
     * public static final Map exceptions =
     *     CollectionUtils.mapify(new Object[] {
     *         new Long(Exception.Interrupt), "Interrupt",
     *         new Long(Exception.TLBModified), "TLBModified",
     *         new Long(Exception.TLBLoad), "TLBLoad"
     * });
     *
     * Note that the implementation uses a LinkedHashMap, so the
     * resulting map will have the same iteration order as the
     * original array.
     *
     * @param  objs array of alternating keys and values
     * @return a modifiable Map containing the key-value associations
     */
    public static Map mapify(Object[] objs) {
        Map result = new LinkedHashMap();
        Debug.assertTrue((objs.length & 1) == 0);
        for (int i = 0; i < objs.length; i+=2) {
            result.put(objs[i], objs[i+1]);
        }
        return result;
    }

    /**
     * Like mapify(), above, but returns an unmodifiable map.  This
     * is useful for initializing constant mappings.
     * @param  objs array of alternating keys and values
     * @return an unmodifiable Map containing the key-value associations
     */
    public static Map constMapify(Object[] objs) {
        return Collections.unmodifiableMap(mapify(objs));
    }

    /**
     * Takes a Map, where the keys are Strings that represent
     * some hierarchy (separated by dots, for example) and produces
     * a new Map where that hierarchy is represented as nested Maps.
     * @param src the original, flat Map, which must have Strings as keys
     * @param delimiter the character to separate src's keys on
     * @return a new Map, where none of the keys contain delimiter,
     *         but instead there is a key for each sub-hierarchy, where
     *         the value is another Map
     * @throws IllegalArgumentException if a node in the hierarchy is
     *         both an interior and leaf node simultaneously
     */
    public static Map makeHierarchicalMap(Map src, char delimiter) {
        Map result = new LinkedHashMap();

        for (Iterator i = src.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            String[] path = StringUtil.split(key, delimiter);
            Map dest = result;
            for (int j = 0; j < path.length - 1; j++) {
                Map nextDest;
                try {
                    nextDest = (Map) dest.get(path[j]);
                } catch (ClassCastException e) {
                    throw new IllegalArgumentException(key);
                }
                if (nextDest == null) {
                    nextDest = new LinkedHashMap();
                    dest.put(path[j], nextDest);
                }
                dest = nextDest;
            }
            if (dest.containsKey(path[path.length - 1])) {
                throw new IllegalArgumentException(key);
            }
            dest.put(path[path.length - 1], entry.getValue());
        }

        return result;
    }

    /**
     * Reads a file of the kind created by mk_instance.  However, unlike
     * com.avlsi.tools.jauto.Floorplan, no additional meaning is
     * assigned to the file.  It is simply returned as a bunch of
     * nested lists.  The lines are processed as follows:
     *
     * A quoted string is always interpreted as a string.
     * Otherwise, if it is "beginlist" or "endlist", it begins or ends
     * a sublist.  If it can be parsed as a BigDecimal, it is interpreted
     * as a BigDecimal.  If all else fails, it is interpreted as a string,
     * and interned.  (Unlike quoted strings, which are not interned.)
     *
     * @param r the file to read from
     * @return a List, which may contain Strings, BigDecimals, and more Lists
     */
    public static List readNestedListFile(BufferedReader r)
        throws IOException {
        List result = new LinkedList();
        String s;

        while ((s = r.readLine()) != null) {
            if (s.startsWith("\"") && s.endsWith("\"")) {
                result.add(s.substring(1, s.length() - 1));
            } else if (s.equals("endlist")) {
                return result;
            } else if (s.equals("beginlist")) {
                result.add(readNestedListFile(r));
            } else {
                Object o;
                try {
                    o = new BigDecimal(s);
                } catch (NumberFormatException e) {
                    o = s.intern();
                }
                result.add(o);
            }
        }
        
        return result;
    }

    /**
     * Imagine you have a bunch of columns of rows.  Each column
     * can contain a different number of rows.  You want to choose one
     * item from each column, and perform an arbitrary function on them.
     * You want to do this for all possible combinations.  That is
     * what this method does.
     * @param  lco  a List of Collections of Objects
     * @param  func an associative binary function to apply to pairs of
     *              Objects
     * @return a Collection of Objects which were returned by the
     *         function
     */
    public static Collection combinations(List lco, BinaryFunction func) {
        assert (lco.size() > 0);
        Collection first = (Collection) lco.iterator().next();
        if (lco.size() == 1)
            return first;
        Collection second = combinations(lco.subList(1, lco.size()), func);
        Collection result = new LinkedList();
        for (Iterator it = first.iterator(); it.hasNext(); ) {
            Object o1 = it.next();
            for (Iterator it2 = second.iterator(); it2.hasNext(); ) {
                Object o2 = it2.next();
                result.add(func.execute(o1, o2));
            }
        }
        return result;
    }

    public static final String _version =
        "$Id$";

    /**
     * Adds all the objects returned by the Iterator to the Collection.
     *
     * @param c the Collection to be added to.
     * @param i the Iterator to drain.
     **/
    public static <T> Collection<T> addAll(Collection<T> c,
                                           Iterator<? extends T> i) {
        while (i.hasNext()) {
            c.add(i.next());
        }
        return c;
    }

    public static <T> Collection<T> addAll(Collection<T> c, T[] o, int off,
                                           int len) {
        for (int i = off; i < off + len; ++i) {
            c.add(o[i]);
        }
        return c;
    }

    public static <T> Collection<T> addAll(Collection<T> c, T... o) {
        return addAll(c, o, 0, o.length);
    }

    public static Iterator sort(Comparator c, Iterator i) {
        return addAll(new MultiSet(c), i).iterator();
    }

    public static <T> Iterable<T> iterable(Iterator<T> it) {
        return new IterableIterator<T>(it);
    }

    public static <T> Iterable<T> iterable(T[]... o) {
        return iterable(new Array2DIterator<T>(o));
    }

    /* This is a method that returns a Collection of Collection. The collection is a partition 
      * formed by transitive closure over any Binary relation. It uses BinaryPredicate to define
      * the relation. 
      * Currently thare are 2 applications of this method.
      * Eg 1: Assign same transistor types to all those half operators that share transistors.
      * This is used in AutoThreshold.java in jauto. It returns a coolection of list of halfoperators that 
      * share transistors. isSharingWith is the relation on which the transitive closure is formed.
      * Eg 2: Assign same variable names to half operators that share transistors and have the same strength group 
      * in TransistorSizingTool. 
      * The method is useful when the relation is transitive.
      * Added on 07/19/2012
    */
    public static<T> Collection<Collection<T>> computeTransitiveClosure(
            Collection<T> coll,
            BinaryPredicate<T,T> relation){
        Collection<Collection<T>> result=
            new ArrayList<Collection<T>>();
        for(final T iT: coll){
            boolean related = false;
            Collection<T> relatedWith = null;
            /*Go through all the elements in the result collection
             */
            for(Iterator its = result.iterator(); its.hasNext();){
                Collection<T> groupT = (Collection) its.next();
                for( final T jT: groupT){
                    /* If the iT element is related with any of the elements
                     * in the result */
                    if(relation.evaluate(iT,jT)){
                        /* This is the first group with which iT is related.
                         * Add iT to this group and remember this group*/
                        if(!related){
                            related = true;
                            relatedWith = groupT;
                            groupT.add(iT);
                        }
                        /* This is no the first group with which iT is related.
                         * So merge this group with the first group that it 
                         * was related with. This step will compute the 
                         * transitive closure.
                         */  
                        else{
                            relatedWith.addAll(groupT);
                            its.remove();
                        }
                        break;


                    }
                }

            }
            /* If iT did not share with any of the elements in the result
             * collection then make a new group and add iT to it.
             * Add this group to the result collection*/
            if(!related){
                Collection<T> newGroup = new ArrayList<T>();
                newGroup.add(iT);
                result.add(newGroup);
            }
        }
        return result;
    }

    /**
     * Return an iterator that iterates over the (at most) first N elements of
     * another iterator.
     **/
    public static<T> Iterator<T> take(final Iterator<? extends T> iter,
                                      final int n) {
        return new Iterator<T>() {
            int count = 0;
            public boolean hasNext() {
                return count < n && iter.hasNext();
            }
            public T next() throws NoSuchElementException {
                if (count < n) {
                    ++count;
                    return iter.next();
                } else {
                    throw new NoSuchElementException();
                }
            }
            public void remove() throws UnsupportedOperationException,
                                        IllegalStateException {
                iter.remove();
            }
        };
    }
}
