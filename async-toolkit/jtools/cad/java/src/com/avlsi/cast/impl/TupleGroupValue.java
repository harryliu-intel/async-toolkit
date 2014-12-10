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

package com.avlsi.cast.impl;

import com.avlsi.cell.CellImpl;
import com.avlsi.util.exception.AssertionFailure;

/**
 * This class bundles tuples together to create one Value for cell
 * instantiation.  Currently it contains the port list and implied
 * port list; if CAST is extended further it might contain more.
 **/

public final class TupleGroupValue extends Value {
    private final TupleValue portList;
    private final TupleValue impliedPortList;

    public TupleGroupValue(TupleValue portList, TupleValue impliedPortList) {
        super(true);
        this.portList = portList;
        this.impliedPortList = impliedPortList;
    }

    public TupleValue getPortList() { return portList; }
    public TupleValue getImpliedPortList() { return impliedPortList; }

    // stuff to implement Value.
    public Value duplicate() {
        throw new AssertionFailure("can't dup tuple groups; can't happen");
    }

    public Value assign   (final Value v, final CellImpl cell)
        throws InvalidOperationException {
        throw new InvalidOperationException("can't assign tuple groups; can't happen");
    }

    public Type getType() throws InvalidOperationException {
        throw new InvalidOperationException("not yet implemented");
    }
}
