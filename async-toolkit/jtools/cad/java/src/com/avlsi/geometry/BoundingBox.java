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
 * This class represents the bounding box of an object in 2D space.
 * Coordinates are doubles.  The class is immutable.
 * The lower left and upper right corners satify the constraints:
 * <code>
 * BoundingBox bb;
 * bb.getLowerLeft().getX() <= bb.getUpperRight().getX()
 *   && bb.getLowerLeft().getY() <= bb.getUpperRight().getY()
 * </code>
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class BoundingBox {
    /**
     * The lower left corner of the bounding box.
     **/
    private final Point lowerLeft;

    /**
     * The upper right corner of the bounding box.
     **/
    private final Point upperRight;

    /**
     * Class constructor.  If the corner arguments do not satisfy
     * the constraints, the x and y values will be swapped so that the
     * constraints are satisfied.
     *
     * @param lowerLeft  lower left corner
     * @param upperRight  upper right corner
     **/
    public BoundingBox(final Point lowerLeft, final Point upperRight)
    {
        if (lowerLeft.getX() <= upperRight.getX()
                && lowerLeft.getY() <= upperRight.getY()) {
            this.lowerLeft = lowerLeft;
            this.upperRight = upperRight;
        }
        else {
            // normalize the coords so that the box is proper
            final double llx, urx;

            if (lowerLeft.getX() < upperRight.getX()) {
                llx = lowerLeft.getX();
                urx = upperRight.getX();
            } else {
                llx = upperRight.getX();
                urx = lowerLeft.getX();
            }

            final double lly, ury;

            if (lowerLeft.getY() < upperRight.getY()) {
                lly = lowerLeft.getY();
                ury = upperRight.getY();
            } else {
                lly = upperRight.getY();
                ury = lowerLeft.getY();
            }

            this.lowerLeft = new Point(llx, lly);
            this.upperRight = new Point(urx, ury);
        }
    }

    /**
     * Class constructor.  If the corner arguments do not satisfy
     * the constraints, the x and y values will be swapped so that the
     * constraints are satisfied.
     *
     * @param lowerLeftX  x coord of lower left corner
     * @param lowerLeftY  y coord of lower left corner
     * @param upperRightX  x coord of upper right corner
     * @param upperRightY  y coord of upper right corner
     **/
    public BoundingBox(final double lowerLeftX,
                       final double lowerLeftY,
                       final double upperRightX,
                       final double upperRightY)
    {
        // normalize the coords so that the box is proper
        final double llx, urx;

        if (lowerLeftX < upperRightX) {
            llx = lowerLeftX;
            urx = upperRightX;
        } else {
            llx = upperRightX;
            urx = lowerLeftX;
        }

        final double lly, ury;

        if (lowerLeftY < upperRightY) {
            lly = lowerLeftY;
            ury = upperRightY;
        } else {
            lly = upperRightY;
            ury = lowerLeftY;
        }

        lowerLeft = new Point(llx, lly);
        upperRight = new Point(urx, ury);
    }

    /**
     * Returns the lower left corner.
     * @return the lower left corner
     **/
    public Point getLowerLeft() {
        return lowerLeft;
    }

    /**
     * Returns the upper right corner.
     * @return the upper right corner
     **/
    public Point getUpperRight() {
        return upperRight;
    }

    /**
     * Returns the x span of the bounding box.
     **/
    public double getWidth() {
        return getUpperRight().getX() - getLowerLeft().getX();
    }

    /**
     * Returns the y span of the bounding box.
     **/
    public double getHeight() {
        return getUpperRight().getY() - getLowerLeft().getY();
    }

    /**
     * Returns the 1/2 Perimeter of the bounding box.
     **/
    public double getHalfPerimeter() {
        return getWidth() + getHeight();
    }

    /**
     * Returns the center of the bounding box.
     **/
    public Point getCenter() {
        return new Point( lowerLeft.getX() + 0.5 * getWidth(), 
                          lowerLeft.getY() + 0.5 * getHeight() );
    }

    public boolean equals(Object o) {
        if (o instanceof BoundingBox) {
            final BoundingBox b = (BoundingBox) o;
            return lowerLeft.equals(b.lowerLeft) &&
                   upperRight.equals(b.upperRight);
        } else {
            return false;
        }
    }
    
    public String toString() {
        return "[" + lowerLeft + " " + upperRight + "]";
    }
}
