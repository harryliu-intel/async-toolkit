// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

import com.avlsi.fast.shapes.Point;

import java.util.NoSuchElementException;

/**
   Iterator iterface used to iterate over all the points in a polygon.
 */
public interface PolygonIterator {
    /**
       Gets the next point in the polygon.  Throws
       NoSuchElementException if there are no more points
       in the polygon.
       @return The next point in the polygon.
     */
    Point next();
    /**
       Determines if there are any more points in the polygon.
       @return true if there are one or more points remaining in the iterator.
     */
    boolean hasNext();
}
