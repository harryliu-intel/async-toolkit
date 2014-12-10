/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

import com.avlsi.fast.shapes.Shape;

import java.util.NoSuchElementException;

/**
   Iterface to an iterator that enumerates all the shapes
   in a collection of shapes.
   @see com.avlsi.fast.shapes.ShapeCollection
 */
public interface WriteableShapeCollectionIterator {
    
    /**
       @return The next shape in the shape collection being iterated.
     */
    Shape next();

    /**
       @return true if the shape collection has more shapes.
     */
    boolean hasNext();

    /**
       Remove the current shape from the collection.
       Depending on the implementation of the collection,
       this iterator may or not be valid after calling
       this method.
     */
    void remove();
}
