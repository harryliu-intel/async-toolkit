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

package com.avlsi.fast.metaparameters;

import com.avlsi.util.exception.AssertionFailure;

/**
 * Class representing arrayed types in metaparameter list.  (Modified from
 * com.avlsi.fast.ports.ArrayType)
 *
 * @author David Hilvert 
 * @version $Revision$ $Date$
 **/
public final class ArrayMetaParam implements MetaParamTypeInterface {
    /** Array of elements **/
    private final MetaParamTypeInterface[] elements;

    /** Minimum index for array. **/
    private final int min;

    /** Maximum index for array. **/
    private final int max;

    /**
     * Class constructor.
     *
     * @param elements Array of element values
     * @param min  minimum index for array
     * @param max  maximum index for array
     **/
    public ArrayMetaParam( final MetaParamTypeInterface[] elements,
            final int min, final int max ){
        this.elements = elements;
        this.min = min;
        this.max = max;

        if( max - min + 1 != elements.length )
            throw new AssertionFailure( 
                    "Array initializer has wrong length" );
    }

    /**
     * Returns arrayed type.
     *
     * @return arrayed type, not null
     **/
    public MetaParamTypeInterface getArrayedType() {
        if( max < min )
            throw new AssertionFailure( "Array has nonpositive size." );

        return elements[0];
    }

    /**
     * Returns an array element.
     *
     * @param index The index of the MetaParamTypeInterface array element 
     *              to return.
     * @return The array element with the specified index.
     **/
    public MetaParamTypeInterface get( int i ){
        return elements[i - min];
    }

    /**
     * Returns minimum index for array.
     *
     * @return minimum index for array
     **/
    public int getMinIndex(){
        return min;
    }

    /**
     * Returns maximum index for array.
     *
     * @return maximum index for array
     **/
    public int getMaxIndex(){
        return max;
    }

    public String toString(){
        return getArrayedType().toString() + '[' + min + ".." + max + ']';
    }
}
