/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;

import com.avlsi.fast.shapes.Point;
import com.avlsi.fast.shapes.WriteablePoint;

import com.avlsi.util.mathexpression.MathExpression;

/**
   Default implementation of WriteablePoint
  */
public class WriteablePointImpl implements WriteablePoint {
    /**
       Coordinates of this point.
     */
    protected MathExpression x, y;

    /**
       Constructs a point from coordinates.
       @param xExp x-coordinate of the point
       @param yExp y-coordinate of the point
     */
    public WriteablePointImpl(final MathExpression xExp,
                              final MathExpression yExp) {
        x = xExp;
        y = yExp;
    }

    /**
       Constructs a point from another point.
       @param point The point to copy.
     */
    public WriteablePointImpl(final Point point) {
        this(point.getX(), point.getY());
    }

    public MathExpression getX() {
        return x;
    }

    public MathExpression getY() {
        return y;
    }

    public void setX(final MathExpression Exp) {
        x = Exp;
    }

    public void setY(final MathExpression Exp) {
        y = Exp;
    }
}
