/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


import com.avlsi.fast.shapes.ShapeCollection;
import com.avlsi.fast.shapes.Shape;
import com.avlsi.fast.shapes.WriteableShapeCollectionIterator;

/**
   Iterface to a collection of shapes.
 */
public interface WriteableShapeCollection extends ShapeCollection {
    /**
       Add a shape to the collection of shapes.
       @param newShape Shape to add to the collection
     */
    void addShape( Shape newShape );

    /**
       @return An iterator that will enumerate all the shapes in the
       collection.
     */
    WriteableShapeCollectionIterator getWriteableIterator();

}
