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
 * This class represents a point with double coordinates in 2D space.
 * Points are treated as column vectors.
 * The class is immutable.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class Point {
    private final double x;
    private final double y;

    public Point (final double x, final double y)
    {
        this.x = x;
        this.y = y;
    }

    /**
     * Returns the x coordinate of the point
     * @return the x coordinate of the point
     **/
    public double getX() { return x; }

    /**
     * Returns the y coordinate of the point
     * @return the y coordinate of the point
     **/
    public double getY() { return y; }

    public boolean equals(Object o) {
        if (o instanceof Point) {
            final Point p = (Point) o;
            return p.x == x && p.y == y;
        } else {
            return false;
        }
    }

    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}
