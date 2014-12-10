/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;


import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.avlsi.fast.shapes.Point;
import com.avlsi.fast.shapes.Polygon;
import com.avlsi.fast.shapes.ShapeIterator;
import com.avlsi.fast.shapes.WriteableShape;

import com.avlsi.fast.shapes.stringexpression.StringExpression;

import com.avlsi.util.mathexpression.MathExpression;

/**
   Default implementation for Shapes.
 */
public class WriteableShapeImpl implements WriteableShape {
    /**
       Names of the layer and the net of this shape.
     */
    protected StringExpression layerName, netName;

    /**
       Origin of the shape.
     */
    protected Point origin;

    /**
       Distances between repetitions along the x and y axes.
     */
    protected MathExpression xRepeat, yRepeat;

    /**
       Name of the repeat boundary layer.
     */
    protected StringExpression boundaryLayer;

    /**
       Storage for the polygons in the shape.
     */
    protected List polygon;

    /**
       Constructs a Shape on the specified layer with the specified net and
       origin.  The shape has no polygons initially.
       @param layerName Name of the layer of the shape.
       @param netName Name of the net of the shape.
       @param origin Origin of the shape.
     */
    public WriteableShapeImpl(final StringExpression layerName,
                     final StringExpression netName,
                     final Point origin,
                     final MathExpression xrep,
                     final MathExpression yrep,
                     final StringExpression boundary) {
        this.layerName = layerName;
        this.netName = netName;
        this.origin = origin;
        this.polygon = new ArrayList();
        this.xRepeat = xrep;
        this.yRepeat = yrep;
        this.boundaryLayer = boundary;
    }

    public StringExpression getLayerName() {
        return layerName;
    }

    public StringExpression getNetName() {
        return netName;
    }

    public Point getOrigin() {
        return origin;
    }

    public MathExpression getXRepeat() {
        return xRepeat;
    }

    public MathExpression getYRepeat() {
        return yRepeat;
    }

    public StringExpression getRepeatBoundaryLayer() {
        return boundaryLayer;
    }

    public ShapeIterator iterator() {
        return new ShapeIterator() {
            private Iterator iterator = polygon.iterator();

            public Polygon next() {
                return (Polygon) iterator.next();
            }

            public boolean hasNext() {
                return iterator.hasNext();
            }
        };
    }

    public void addPolygon(Polygon poly) {
        polygon.add(poly);
    }
}
