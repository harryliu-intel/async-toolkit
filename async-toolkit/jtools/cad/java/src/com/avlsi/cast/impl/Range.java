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

package com.avlsi.cast.impl;

/**
 * Range class, for array subscripts and loop indices.
 * Currently supports only increasing, consecutive ranges.  Will
 * be amended to support funkier ranges.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Range {
    private final int min;
    private final int max;

    public Range(final int min, final int max) {
        this.min = min;
        this.max = max;
    }

    public int size() {
        return max - min + 1;
    }

    public boolean contains(final int i) {
        return min <= i && i <= max;
    }

    public int getMin() {
        return min;
    }

    public int getMax() {
        return max;
    }

    public Iterator iterator() {
        return new Iterator();
    }

    public final class Iterator {
        private int cur = min;

        public boolean hasNext() {
            return cur <= max;
        }

        public int next() {
            return cur++;
        }

    }

    public String toString() {
        return min + ".." + max;
    }

    public boolean equals(final Object o) {
        if (!(o instanceof Range))
            return false;
        else
            return equals((Range) o);

    }

    public boolean equals(final Range r) {
        return min == r.min && max == r.max;
    }
}
