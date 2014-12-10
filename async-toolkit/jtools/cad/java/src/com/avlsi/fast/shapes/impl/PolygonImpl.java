/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;

import com.avlsi.fast.shapes.Point;
import com.avlsi.fast.shapes.Polygon;
import com.avlsi.fast.shapes.PolygonIterator;

import java.util.Collection;
import java.util.NoSuchElementException;

/**
   Default implementation for Polygon.  Points are stored in an array to make
   this class relatively cheap.  There is no way to modify the polygon after
   it's been constructed.
 */
public class PolygonImpl implements Polygon {
    /**
       Storage for points.
     */
    protected Point[] points;

    /**
       Construct a polygon from an array of Points.
       @see com.avlsi.fast.shapes.Point
       @param points Array of Points to construct the polygon from.
     */
    public PolygonImpl(final Point[] points) {
        this.points = new Point[points.length];
        System.arraycopy(points, 0, this.points, 0, points.length);
    }

    /**
       Construct a polygon from a Collection of Points.
       @see com.avlsi.fast.shapes.Point
       @param c Collection of Points to construct the polygon from.
     */
    public PolygonImpl(final Collection c) {
        points = (Point[]) c.toArray(new Point[0]);
    }

    public PolygonIterator iterator() {
        return new PolygonIterator() {
            private int index = 0;
            
            public Point next() {
                if (index < points.length) {
                    return points[index++];
                } else {
                    throw(new NoSuchElementException());
                }
            }

            public boolean hasNext() {
                return index < points.length;
            }
        };
    }

    public int size() {
        return points.length;
    }
}
