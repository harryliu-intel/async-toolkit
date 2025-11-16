// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

import com.avlsi.fast.shapes.Polygon;

import java.util.NoSuchElementException;

/**
   Iterator interface used to iterate through
   all the polygons in a shape.
   @see com.avlsi.fast.shapes.Point
 */
public interface ShapeIterator {
    /**
       Returns the next polygon in the shape being iterated.
       @return the next polygon in the shape being iterated.
     */
    Polygon next();
    /**
       Returns true if the shape has more polygons.
       @return true if the shape has more polygons.
     */
    boolean hasNext();
}
