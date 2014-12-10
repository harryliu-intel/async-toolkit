/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;


import java.util.Collection;

import com.avlsi.fast.shapes.ShapeFactory;
import com.avlsi.fast.shapes.ShapeFunction;
import com.avlsi.fast.shapes.Point;
import com.avlsi.fast.shapes.Polygon;
import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.impl.ShapeFunctionImpl;
import com.avlsi.fast.shapes.impl.WriteablePointImpl;
import com.avlsi.fast.shapes.impl.PolygonImpl;

import com.avlsi.util.mathexpression.MathExpression;

public class ShapeFactoryImpl implements ShapeFactory {
    public ShapeFunction makeShapeFunction(final String name,
                                           final StringExpression layer,
                                           final StringExpression net,
                                           boolean repeatx, boolean repeaty,
                                           final StringExpression boundary,
                                           final Point origin,
                                           String[] mathParams,
                                           String[] stringParams) {
        return new ShapeFunctionImpl(name, layer, net, repeatx, repeaty,
                                     boundary, origin, mathParams,
                                     stringParams);
    }

    public Point makePoint(final MathExpression xExp,
                           final MathExpression yExp) {
        return new WriteablePointImpl(xExp, yExp);
    }

    public Polygon makePolygon(final Collection c) {
        return new PolygonImpl(c);
    }
}
