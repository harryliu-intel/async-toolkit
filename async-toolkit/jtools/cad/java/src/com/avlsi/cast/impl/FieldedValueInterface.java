/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

/**
 * Interface for Values which can have fields accessed.
 **/
public interface FieldedValueInterface {
    int ALL_PERMISSION = 0;
    int INSTANCE_PERMISSION = 1;
    Value accessField(final Symbol sym, final int permission)
        throws InvalidOperationException;
}
