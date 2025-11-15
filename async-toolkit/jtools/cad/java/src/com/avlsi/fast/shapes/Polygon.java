// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

/**
   Interface to a polygon.
 */
public interface Polygon {
    /**
       @return An iterator that will iterate over all the points in the
       polygon.
     */
    PolygonIterator iterator();

    /**
       @return Returns the number of points in the polygon.
     */
    int size();
}
