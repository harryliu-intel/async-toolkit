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

import java.util.Map.Entry;

/**
 * Container class for two objects.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 * @review kiniry 23 July 2002
 **/
public class Pair<P,Q> implements Comparable<Pair<P,Q>> {
    private final P first;
    private final Q second;

    public Pair(final P first, final Q second) {
        this.first = first;
        this.second = second;
    }

    public P getFirst() {
        return first;
    }

    public Q getSecond() {
        return second;
    }

    /**
     * Converts a Map.Entry into a Pair.
     **/
    public static <K,V> Pair<K,V> fromMapEntry(final Entry<K,V> e) {
        return new Pair<K,V>(e.getKey(), e.getValue());
    }

    //
    // extends Object
    //

    public String toString() {
        return "(" + getFirst() + ", " + getSecond() + ")";
    }

    public boolean equals(final Object o) {
        return o instanceof Pair && equals((Pair) o);
    }

    public boolean equals(final Pair p) {
        return ObjectUtils.equals(getFirst(), p.getFirst()) &&
               ObjectUtils.equals(getSecond(), p.getSecond());

    }

    public int hashCode() {
        return ObjectUtils.hashCode(getFirst()) ^
              ~ObjectUtils.hashCode(getSecond());
    }

    //
    // implements Comparable
    //

    public int compareTo(final Pair<P,Q> p) {
        final int c = ObjectUtils.compare((Comparable) getFirst(),
                                          (Comparable) p.getFirst());

        if (c != 0)
            return c;
        else
            return ObjectUtils.compare((Comparable) getSecond(),
                                       (Comparable) p.getSecond());
    }
}
