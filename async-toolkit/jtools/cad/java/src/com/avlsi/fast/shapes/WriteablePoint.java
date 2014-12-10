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
 */
public interface WriteablePoint extends Point {
    /**
       Sets the expression for the x-coordinate of the point.
       @param Exp The expression for the x-coordinate of the point.
     */
    void setX( MathExpression Exp );
    /**
       Sets the expression for the x-coordinate of the point.
       @param Exp The expression for the x-coordinate of the point.
     */
    void setY( MathExpression Exp );
}
