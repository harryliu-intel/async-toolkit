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

package com.avlsi.geometry;

/**
 * Class to represent steinner trees.
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public abstract class SteinnerTree {
    /**
     * constructor surrogate.
     *
     * The two arrays should be of the same size represent the x and
     * y co&ouml;rdinates of the points to be joined in a Steinner tree.
     *
     * Should be static, but ...
     **/
    public abstract SteinnerTree factory(float x[], float y[]);
    /**
     * @return the sum of the lengths of all segments of the tree.
     * @throws ArrayIndexOutOfBoundsException on differing array sizes.
     **/
    public abstract float getTotalLength();
    /**
     * @return the sum of the lengths of the segments of the path connecting the two
     * points.
     *
     * @throws ArrayIndexOutOfBoundsException on 
     **/
    public abstract float getLength(int index1, int index2);
    /**
     * @return number of points we are supposed to cover.
     **/
    public abstract int getInitialPointCount();
    /**
     * @return number of points added to reduce total length.
     **/
    public abstract int getAddedPointCount();
    /**
     * @return x co&oumlrdinate of point number <code>index</code>
     * @throws ArrayIndexOutOfBoundsException on <code>index &gt;= getInitialPointCount() + getAddedPointCount();</code>.
     */
    public abstract float getX(int index);
    /**
     * @return y co&oumlrdinate of point number <code>index</code>
     * @throws ArrayIndexOutOfBoundsException on <code>index &gt;= getInitialPointCount() + getAddedPointCount();</code>.
     */
    public abstract float getY(int index);
}
