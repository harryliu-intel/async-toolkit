/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

import com.avlsi.fast.shapes.ShapeCollectionIterator;

/**
   Iterface to a collection of shapes.
   @see com.avlsi.fast.shapes.Shape
 */
public interface ShapeCollection {
    /**
       @return An interator that will iterate through all the shapes
       in the collection.
     */
    ShapeCollectionIterator iterator();
}
