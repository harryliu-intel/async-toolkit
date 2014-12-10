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
 * Transformation class to transform {@link Point}s, and
 * {@link BoundingBox}es.  The transformation is a matrix that
 * premultiplies points, which are column vectors.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class Transform {
    private final double[][] forward;
    private final double[][] inverse;

    /**
     * Class constructor.  The matrix of the transform will be:
     * <tt>
     * a b c<br>
     * d e f<br>
     * 0 0 1
     * </tt>
     **/
    public Transform(final double a, final double b, final double c,
                     final double d, final double e, final double f)
    {
        forward = new double[][] {
            { a, b, c },
            { d, e, f },
            { 0, 0, 1 }, };

        final double det = a * e - b * d;
        inverse = new double[][] {
            {  e / det , -b / det , (b * f - c * e) / det },
            { -d / det ,  a / det , (c * d - a * f) / det },
            {     0    ,     0    ,            1          }, };
    }

    /**
     * Transform a point.
     * @param p  point to be transformed
     * @return  the transformed point
     **/
    public Point transform(final Point p) {
        return transform(forward, p);
    }

    /**
     * Transform a point, using the inverse transformation.
     * @param p  point to be transformed
     * @return  the transformed point
     **/
    public Point inverseTransform(final Point p) {
        return transform(inverse, p);
    }

    /**
     * Transform a point, using the specified transformation matrix.
     * @param t  the transformation matrix
     * @param p  point to be transformed
     * @return  the transformed point
     **/
    private Point transform(final double[][] t, final Point p) {
        final double px = p.getX();
        final double py = p.getY();

        final double x = t[0][0] * px + t[0][1] * py + t[0][2];
        final double y = t[1][0] * px + t[1][1] * py + t[1][2];
        final double w = t[2][0] * px + t[2][1] * py + t[2][2];

        return new Point(x / w, y / w);
    }

    /**
     * Transform a bounding box.
     * @param bb  bounding box to be transformed
     * @return  the transformed bounding box
     **/
    public BoundingBox transform(final BoundingBox bb) {
        return new BoundingBox(
                transform(bb.getLowerLeft()),
                transform(bb.getUpperRight()));
    }

    /**
     * Transform a bounding box, using the inverse transformation.
     * @param bb  bounding box to be transformed
     * @return  the transformed bounding box
     **/
    public BoundingBox inverseTransform(final BoundingBox bb) {
        return new BoundingBox(
                inverseTransform(bb.getLowerLeft()),
                inverseTransform(bb.getUpperRight()));
    }
}
