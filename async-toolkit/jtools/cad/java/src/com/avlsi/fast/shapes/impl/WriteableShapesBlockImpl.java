/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;

import com.avlsi.fast.shapes.impl.WriteableShapeCollectionImpl;
import com.avlsi.fast.shapes.Shape;
import com.avlsi.fast.shapes.ShapeCollection;
import com.avlsi.fast.shapes.ShapeCollectionIterator;
import com.avlsi.fast.shapes.WriteableShapeCollection;
import com.avlsi.fast.shapes.WriteableShapeCollectionIterator;
import com.avlsi.fast.shapes.WriteableShapesBlock;

/**
   Default implementation of WriteableShapesBlock.
 */
public class WriteableShapesBlockImpl implements WriteableShapesBlock {
    /**
       Storage for shapes in the collection.
     */
    protected WriteableShapeCollection collection;

    /**
       Constructs an empty block containing a collection with an initial size
       of 4.
     */
    public WriteableShapesBlockImpl() {
        this(4);
    }

    /**
       Constructs an empty block containing a collection with the given initial
       size.
       @param size Initial size of the block.
     */
    public WriteableShapesBlockImpl(final int size) {
        collection = new WriteableShapeCollectionImpl(size);
    }

    /**
       Constructs a block containing the specified collection.
       @param c A collection to construct a block from.
     */
    public WriteableShapesBlockImpl(final ShapeCollection c) {
        this();
        addShapes(c);
    }

    public ShapeCollection getShapes() {
        return collection;
    }

    public void addShapes(ShapeCollection NewShapes) {
        ShapeCollectionIterator iter = NewShapes.iterator();
        while (iter.hasNext()) {
            collection.addShape(iter.next());
        }
    }

    public WriteableShapeCollection getWriteableShapes() {
        return collection;
    }
}
