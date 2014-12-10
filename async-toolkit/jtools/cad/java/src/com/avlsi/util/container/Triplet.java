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

/**
 * Container class for three objects.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class Triplet<P,Q,R> implements Comparable<Triplet<P,Q,R>> {
    private final P first;
    private final Q second;
    private final R third;

    public Triplet(final P first, final Q second, final R third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }

    public P getFirst() {
        return first;
    }

    public Q getSecond() {
        return second;
    }

    public R getThird() {
        return third;
    }

    //
    // extends Object
    //

    public String toString() {
        return "(" + getFirst() +
            ", " + getSecond() +
            ", " + getThird() +
            ")";
    }

    public boolean equals(final Object o) {
        return o instanceof Triplet && equals((Triplet) o);
    }

    public boolean equals(final Triplet t) {
        return ObjectUtils.equals(getFirst(), t.getFirst()) &&
               ObjectUtils.equals(getSecond(), t.getSecond()) &&
               ObjectUtils.equals(getThird(), t.getThird());

    }

    public int hashCode() {
        return ObjectUtils.hashCode(getFirst()) ^
              ~ObjectUtils.hashCode(getSecond()) ^
              (ObjectUtils.hashCode(getThird()) << 16);
    }

    //
    // implements Comparable
    //
    public int compareTo(final Triplet<P,Q,R> t) {
        final int c1 = ObjectUtils.compare((Comparable) getFirst(),
                (Comparable) t.getFirst());

        if (c1 != 0)
            return c1;
        else {
            final int c2 = ObjectUtils.compare((Comparable) getSecond(),
                    (Comparable) t.getSecond());

            if (c2 != 0)
                return c2;
            else
                return ObjectUtils.compare((Comparable) getThird(),
                                           (Comparable) t.getThird());
        }
    }
}
