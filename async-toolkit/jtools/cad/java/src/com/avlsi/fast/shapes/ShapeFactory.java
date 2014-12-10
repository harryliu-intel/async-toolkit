/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


import java.util.Collection;

import com.avlsi.fast.shapes.ShapeFunction;
import com.avlsi.fast.shapes.Point;
import com.avlsi.fast.shapes.Polygon;
import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.util.mathexpression.MathExpression;

public interface ShapeFactory {
    ShapeFunction makeShapeFunction(final String name,
                                    final StringExpression layer,
                                    final StringExpression net,
                                    boolean repeatx, boolean repeaty,
                                    final StringExpression boundary,
                                    final Point origin,
                                    String[] mathParams,
                                    String[] stringParams);

    Point makePoint(final MathExpression xExp,
                    final MathExpression yExp);

    Polygon makePolygon(final Collection c);
}
