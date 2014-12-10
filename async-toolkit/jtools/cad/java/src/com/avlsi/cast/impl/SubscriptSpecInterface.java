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

import com.avlsi.tools.cosim.CoSimChannelNames;

/**
 * This class represents subscripting specification for arrays,
 * ie <code>[K..L,M..N]</code> for dense arrays or even sparse
 * arrays with indices <code>[0,0], [1,2], [3,1]</code> defined.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public interface SubscriptSpecInterface {
    /**
     * Returns the number of elements in the subscript specification.
     **/
    int getNumElements();

    /**
     * Returns the number of dimensions in the subscript specification.
     **/
    int getNumDimensions();

    /**
     * Returns the position in a 1-D array indexed by this
     * SubscriptSpec at which the specified element will be found.
     * The order is row-major. positionOf(new int[]{1,2}) means
     * a[1,2].
     *
     * @exception IllegalArgumentException
     *   if <code>idx.length != getNumDimensions()</code>
     * @exception IndexOutOfBoundsException
     *   if <code>idx</code> is not a subscript contained in the
     *   subscript specification
     **/
    int positionOf(final int[] idx);

    /**
     * Returns the subscripted spec for the given position.
     *
     * @exception ArrayIndexOutOfBoundsException
     *   if <code>pos < 0 || pos >= getNumElements()</code>
     **/
    int[] indexOf(int pos);

    /**
     * Returns an array of names.  Under the cosim framework, each of
     * these names corresponds to a NodeChannel.
     *
     * @param baseName the name of the overall array
     * @param ignoreLastDimension Arrays of wide channels use the last
     * dimension as channelwidth, so cosim names for that array should
     * ignore that dimension
     **/
    CoSimChannelNames getCoSimChannelNames(String baseName,
                                           boolean ignoreLastDimension);
}
