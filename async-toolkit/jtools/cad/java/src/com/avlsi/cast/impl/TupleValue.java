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

import java.util.Arrays;
import java.util.Iterator;

import com.avlsi.cell.CellImpl;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

/**
 * This class represents values for tuple expressions.
 * These only occur in instantiation expressions:  
 * <code>T(v1,v2,v3) x(v4,v5,v6)</code>.  <code>(v1,v2,v3)</code>
 * and <code>(v4,v5,v6)</code> are tuple values. Assignments to
 * tuples assign to the contained values.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TupleValue extends Value {

    private final Value[] vals;

    public TupleValue(final Value[] vals) {
        super(true);
        this.vals = vals;
    }

    public static TupleValue valueOf(final Value val)
        throws InvalidOperationException {
        if (val instanceof TupleValue)
            return (TupleValue) val;
        else
            throw new InvalidOperationException("no conversion of " 
                                                + val.getInstanceName() +
                                                " to tuple");
    }

    public Value duplicate() {
        throw new AssertionFailure("can't dup tuples; can't happen");
    }

    public int getSize() {
        return vals.length;
    }

    // container
    public Value accessTuple(int i) throws InvalidOperationException {
        try {
            return vals[i];
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new InvalidOperationException("Index out of bounds: tried to access a tuple of length " + vals.length + " with index " + i, e);
        }
    }

    // assignment
    public Value assign   (final Value v, final CellImpl cell)
        throws InvalidOperationException {
        final TupleValue tv = valueOf(v);

        if (tv.vals.length != vals.length)
            throw new InvalidOperationException(
                    "sizes don't agree for assignment: " + tv.vals.length
                    + "!=" + vals.length);

        for (int i = 0; i < vals.length; ++i)
            vals[i].assign(tv.vals[i], cell);

        return this;
    }

    public Type getType() throws InvalidOperationException {
        throw new InvalidOperationException("not implemented yet");
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("(");
        for (int i = 0; i < vals.length; ++i) {
            if (i > 0)
                sb.append(",");
            sb.append(vals[i].toString());
        }
        sb.append(")");

        return sb.toString();
    }

    /**
     * Returns true if all of the values in the tuple are
     * equal according to their equals methods.
     **/
    public boolean equals(final Object o) {
        if (!(o instanceof TupleValue))
            return false;
        else
            return equals((TupleValue) o);
    }

    /**
     * Returns true if all of the values in the tuple are
     * equal according to their equals methods.
     **/
    public boolean equals(final TupleValue tv) {
        if (getSize() != tv.getSize())
            return false;

        for (int i = 0; i < getSize(); ++i)
            if (!vals[i].equals(tv.vals[i]))
                return false;

        return true;
    }

    public int hashCode() {
        int hc = 0;

        for (int i = 0; i < getSize(); ++i)
            hc ^= vals[i].hashCode();

        return hc;
    }

    /**
     * Iterate over the values in the tuple, in order, starting from the first
     * element.
     **/
    public Iterator getIterator() {
        return Arrays.asList(vals).iterator();
    }
}
