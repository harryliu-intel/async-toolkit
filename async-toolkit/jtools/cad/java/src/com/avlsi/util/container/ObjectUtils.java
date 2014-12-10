/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

/**
 * Utilites operating on Objects.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ObjectUtils {
    /**
     * This class should not be instantiated.
     **/
    private ObjectUtils() { }

    /**
     * Tests for equality using o1.equals(), but handling o1 == null case.
     **/
    public static boolean equals(final Object o1, final Object o2) {
        if (o1 == null) {
            return o2 == null;
        } else {
            return o1.equals(o2);
        }
    }

    /**
     * Hashes the object.  Handles null.
     **/
    public static int hashCode(final Object o) {
        if (o == null)
            return 0;
        else
            return o.hashCode();
    }

    /**
     * Compares, considering null < o, if o is not null.
     **/
    public static <T extends Comparable<T>> int compare(T o1, T o2) {
        if (o1 == null) {
            if (o2 == null)
                return 0;
            else
                return -1;
        } else {
            if (o2 == null)
                return 1;
            else
                return o1.compareTo(o2);
        }
    }

    public static <T extends Comparable<T>> int compare(T... o) {
        for (int i = 0; i < o.length; i += 2) {
            final int comp = compare(o[i], o[i + 1]);
            if (comp != 0) return comp;
        }
        return 0;
    }
}
