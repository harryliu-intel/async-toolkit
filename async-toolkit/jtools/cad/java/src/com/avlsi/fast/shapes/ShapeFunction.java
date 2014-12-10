/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

import java.util.Set;

import com.avlsi.fast.shapes.ShapeCollection;
import com.avlsi.fast.shapes.ShapeFunctionTable;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.util.mathexpression.ExpressionCollection;

/**
   Interface to a compiled ShapeFunction.  A ShapeFunction contains a list of
   statements.  A statement is either a list of points, or a call to another
   ShapeFunction.
 */
public interface ShapeFunction {
    /**
       Gets the name of the function.
       @return Name of the function.
     */
    String getName();

    /**
       Recursively evaluates this function so that all calls to other shape
       functions are removed.
       @param table A map from String to ShapeFunction, used to look up
       function calls.
       @param seen A Set of function names that should not be called again, to
       avoid recursion (since there is no control statements in the Shapes
       language).
       @return All the shapes that this ShapeFunction generated in a
       ShapeCollection.
     */
    ShapeCollection resolve(final ShapeFunctionTable table,
                            final Set seen,
                            final ExpressionCollection mathArgs,
                            final StringExpressionCollection stringArgs);

    /**
       Recursively evaluates the anonymous function returned by the parser.
       @param table A map from String to ShapeFunction, used to look up
       function calls.
       @return All the shapes that this ShapeFunction generated in a
       ShapeCollection.
     */
    ShapeCollection resolve(final ShapeFunctionTable table);

    /**
       Add a function call to the body of this function.
       @param name Name of the function to call.
       @param mathArgs Arguments to be bound to MathExpressions.
       @param stringArgs Arguments to be bound to StringExpressions.
     */
    void addFunctionCall(final String name,
                         final ExpressionCollection mathArgs,
                         final StringExpressionCollection stringArgs);

    /**
       Add a polygon to the body of this function.
       @param poly Polygon to add.
     */
    void addPolygon(final Polygon poly);
}
