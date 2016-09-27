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

package com.avlsi.csp.csp2java.runtime;

import java.util.function.Function;

import com.avlsi.csp.csp2java.runtime.Fold.BinaryFunction;

/**
 * A class for runtime array support of any number of dimensions.  The object
 * being stored inside the array must have a default constructor.
 * 
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class CspArray implements CspCloneableValue, Packable {
    /**
     * Storage for the elements in the array.
     **/
    final CspValue[] ca;

    /**
     * The range of the array.
     **/
    final int min, max;

    /**
     * Class constructor.
     *
     * @param min lower bound of the array
     * @param max upper bound of the array
     * @param elem class of the element to store inside the array
     **/
    public CspArray (final CspInteger min, final CspInteger max,
                     final Class elem) {
        this.min = min.intValueExact();
        this.max = max.intValueExact();

        ca = new CspValue[this.max - this.min + 1];

        try {
            for (int i = 0; i < ca.length; ++i)
                ca[i] = (CspValue) elem.newInstance();
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    CspValue get (final int index) {
        return ca[index - min];
    }

    /**
     * Get the object stored at the given index.
     *
     * @param index index of the element to retrieve
     * @return object stored at <var>index</var>
     **/
    public CspValue get (final CspInteger index) {
        return get(index.intValueExact());
    }

    /**
     * Get the object stored at the given index.  If it is out of bounds,
     * print a pretty error message and exit.
     *
     * @param index      index of the element to retrieve
     * @param filename   source file that the error occurred in
     * @param line       source line number that the error occurred on
     * @param column     source column number that the error occurred at
     * @return           object stored at <var>index</var>
     **/
    public CspValue get (final CspInteger index, String filename,
                         int line, int column) {
        try {
            return get(index);
        } catch (ArrayIndexOutOfBoundsException e) {
            throw (CspArrayBoundsException)
                  new CspArrayBoundsException(index, min, max, filename, line,
                                              column).initCause(e);
        }
    }

    public void setValue(final CspValue v) {
        final CspArray array = (CspArray) v;
        for (int i = 0; i < ca.length; ++i) {
            ca[i].setValue(array.ca[i]);
        }
    }

    public void setValue(CspValue v, BinaryFunction modifier) {
        throw new UnsupportedOperationException();
    }

    public CspCloneableValue duplicate() {
        return duplicate(v -> v.duplicate());
    }

    public CspCloneableValue duplicate(
            Function<CspCloneableValue,CspValue> elemMapFunc) {
        final CspValue[] dup = new CspValue[ca.length];
        for (int i = 0; i < ca.length; ++i)
            dup[i] = elemMapFunc.apply((CspCloneableValue) ca[i]);
        return new CspArray(min, max, dup);
    }

    public int getMinIndex() {
        return min;
    }

    public int getMaxIndex() {
        return max;
    }

    /**
     * Construct an array initialized with <code>ca</code>.  <code>ca</code> is
     * stored as a reference, and its content is <b>not</b> copied.  Care
     * should be taken to avoid modifying it.
     **/
    CspArray (final int min, final int max, final CspValue[] ca) {
        assert max - min + 1 == ca.length;
        this.min = min;
        this.max = max;
        this.ca = ca;
    }

    private static CspArray makeArray(final CspInteger[] limits,
                                      int start, final Class elem) {
        if (limits.length - start == 2) {
            return new CspArray(limits[start], limits[start + 1], elem);
        } else {
            final int min = limits[start].intValueExact();
            final int max = limits[start + 1].intValueExact();
            final long llen = (long) max - min + 1;
            if (llen > Integer.MAX_VALUE) {
                throw new ArithmeticException("Array too large (" + llen + " > 2^31-1)");
            }
            final int len = (int) llen;
            final CspValue[] content = new CspValue[len];
            for (int i = 0; i < len; ++i) {
                content[i] = makeArray(limits, start + 2, elem);
            }
            return new CspArray(min, max, content);
        }
    }

    /**
     * Construct a possibly multi-dimensional array.  If the array is
     * multidimensional, the result is a <code>CspArray</code> that contains
     * <code>CspArray</code>s of one less dimension.
     *
     * @param limits bounds of the dimensions of the array;
     * <code>limits[0]</code> and <code>limits[1]</code> are the lower and
     * upper bound of the first dimension respectively, <code>limits[2]</code>
     * and <code>limits[3]</code> are the lower and upper bound of the second
     * dimension, and so on.  Therefore the length of <var>limits</var> must be
     * even.
     * @param elem class of the element to store inside the array
     *
     * @return a CspArray with the specified dimensions, with all elements
     * initialized using the default constructor of <var>elem</var>
     **/
    public static CspArray makeArray(final CspInteger[] limits,
                                     final Class elem) {
        // assert limits.length % 2 == 0;
        return makeArray(limits, 0, elem);
    }

    public int pack(final CspInteger packed, int start) {
        for (CspValue val : ca) {
            start = ((Packable) val).pack(packed, start);
        }
        return start;
    }

    public int unpack(final CspInteger unpacked, int start) {
        for (CspValue val : ca) {
            start = ((Packable) val).unpack(unpacked, start);
        }
        return start;
    }
}
