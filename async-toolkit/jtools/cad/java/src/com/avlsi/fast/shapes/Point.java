/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.fast.shapes;

import com.avlsi.util.mathexpression.MathExpression;

/**
   Interface to a point used in the shapes block code.
   @see com.avlsi.fast.shapes.Shape
   @see com.avlsi.fast.shapes.ShapesBlock
 */
public interface Point {
    /**
       Gets the expression for the x-coordinate of the point.
       @return A MathExpression for the x-coordinate of the point.
     */
    MathExpression getX( );
    /**
       Gets the expression for the y-coordinate of the point.
       @return A MathExpression for the y-coordinate of the point.
     */
    MathExpression getY( );
}
