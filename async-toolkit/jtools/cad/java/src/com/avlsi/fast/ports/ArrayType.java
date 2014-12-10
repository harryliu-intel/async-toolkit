/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast.ports;

/**
 * Class representing arrayed types in port list.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class ArrayType implements PortTypeInterface {

    /** Type being arrayed, not null. **/
    private final PortTypeInterface arrayedType;

    /** Minimum index for array. **/
    private final int min;

    /** Maximum index for array. **/
    private final int max;

    /**
     * Class constructor.
     *
     * @param arrayedType  type being arrayed
     * @param min  minimum index for array
     * @param max  maximum index for array
     **/
    public ArrayType(final PortTypeInterface arrayedType,
            final int min, final int max) {
        this.arrayedType = arrayedType;
        this.min = min;
        this.max = max;
    }

    /**
     * Returns arrayed type.
     *
     * @return arrayed type, not null
     **/
    public PortTypeInterface getArrayedType() {
        return arrayedType;
    }

    /**
     * Returns minimum index for array.
     *
     * @return minimum index for array
     **/
    public int getMinIndex() {
        return min;
    }

    /**
     * Returns maximum index for array.
     *
     * @return maximum index for array
     **/
    public int getMaxIndex() {
        return max;
    }

    public String toString() {
        return arrayedType.toString() + '[' + min + ".." + max + ']';
    }
}
