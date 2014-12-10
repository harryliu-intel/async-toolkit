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

import com.avlsi.cell.CellImpl;
import com.avlsi.util.exception.AssertionFailure;

/**
 * This class is just a tagging class, none of its methods should ever
 * be called.  AnonymousValues can appear inside expression lists
 * for the port list initializer.  The only operation that will
 * be performed on an AnonymousValue is an instanceof check in
 * InstanceValue.assign, for the TupleValue case.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class AnonymousValue extends Value {

    /**
     * Constructor.
     **/
    public AnonymousValue() {
        super(true);
    }

    public Value assign(final Value v, final CellImpl cell) {
        throw new AssertionFailure("Can't call assign() on an anonymous value.");
    }

    public Value duplicate() {
        throw new AssertionFailure("Can't call duplicate() on an anonymous value.");
    }

    public Type getType() {
        throw new AssertionFailure("Can't call getType() on an anonymous value.");
    }
}
