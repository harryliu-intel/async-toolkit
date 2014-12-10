/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.fast.shapes;

import com.avlsi.fast.shapes.ShapeIterator;
import com.avlsi.fast.shapes.Point;


import com.avlsi.fast.shapes.stringexpression.StringExpression;

import com.avlsi.util.mathexpression.MathExpression;

/**
   Interface to a shape.
 */
public interface Shape {
    /**
       @return A string expression for the name of the layer the shape
       is to be drawn on.
    */
    StringExpression getLayerName( );

    /**
       @return A string expression which evaluates to the name
       of the net the shape should be attached to or null if the
       shape should not be attached to any net.
     */
    StringExpression getNetName( );

    /**
       @return An iterator that will iterate over all the polygons
       in the shape.
     */
    ShapeIterator iterator();

    /**
       @return The location of the shape's origin
       in the refering cell.
     */
    Point getOrigin();

    /**
       @return null if the shape is not repeated along the x-axis,
       otherwise an expression that evaluates to the distance
       along the x-axis between repitions of a repeated shape.
       When this method does not return null, then the x-coordinate
       of the point returned from getOrigin should be ignored.
       The left most repition should put the x-coordinate of its origin
       at the left edge of the shape on the repeat boundary layer.
       The last repition should be entirely entirely enclosed by the
       shape on the boundary layer.
     */
    MathExpression getXRepeat();
    /**
       @return null if the shape is not repeated along the y-axis,
       otherwise an expression that evaluates to the distance
       along the y-axis between repitions of a repeated shape.
       When this method does not return null, then the y-coordinate
       of the point returned from getOrigin should be ignored.
       The bottom most repition should put the y-coordinate of its origin
       at the bottom edge of the shape on the repeat boundary layer.
       The last repition should be entirely entirely enclosed by the
       shape on the boundary layer.
     */
    MathExpression getYRepeat();

    /**
       @return A string expression that evaluates to the name
       of the repeat boundary layer.  
     */
    StringExpression getRepeatBoundaryLayer();


}
