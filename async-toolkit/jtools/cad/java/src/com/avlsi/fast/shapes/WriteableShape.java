// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
   Interface to a writeable shape.
 */
public interface WriteableShape extends Shape {
    /**
       Add a polygon to this shape.
     */
    void addPolygon(Polygon poly);
}
