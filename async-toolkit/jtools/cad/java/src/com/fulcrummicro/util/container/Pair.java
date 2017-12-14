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

package com.fulcrummicro.util.container;

/**
 * Container class for two objects.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 * @review kiniry 23 July 2002
 **/
public class Pair<P, Q> {

    private final P first;

    private final Q second;

    public Pair(final P first, final Q second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean equals(final Object o) {
        Pair<?, ?> p;

        if (o instanceof Pair<?, ?>) {
            p = (Pair<?, ?>) o;
            return equals(getFirst(), p.getFirst())
                   && equals(getSecond(), p.getSecond());
        }
        return false;
    }

    public P getFirst() {
        return this.first;
    }

    public Q getSecond() {
        return this.second;
    }

    @Override
    public int hashCode() {
        return hashCode(this.getFirst()) ^ ~hashCode(this.getSecond());
    }

    @Override
    public String toString() {
        return "(" + getFirst() + ", " + getSecond() + ")";
    }

    /**************************************************************************
     * Private Methods
     **************************************************************************/

    private static boolean equals(final Object o1, final Object o2) {
        if (o1 == null) {
            return o2 == null;
        } else {
            return o1.equals(o2);
        }
    }

    private static int hashCode(final Object o) {
        if (o == null) {
            return 0;
        } else {
            return o.hashCode();
        }
    }

}
