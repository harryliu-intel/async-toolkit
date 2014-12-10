/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.avlsi.fast.shapes.Shape;
import com.avlsi.fast.shapes.ShapeCollection;
import com.avlsi.fast.shapes.ShapeCollectionIterator;
import com.avlsi.fast.shapes.WriteableShapeCollection;
import com.avlsi.fast.shapes.WriteableShapeCollectionIterator;

/**
   Default implementation of WriteableShapeCollection.  Shapes are stored in an
   ArrayList.
 */
public class WriteableShapeCollectionImpl implements WriteableShapeCollection {
    /**
       Storage for shapes in the collection.
     */
    protected List shapes;

    /**
       Constructs a collection with an initial size of 4.
     */
    public WriteableShapeCollectionImpl() {
        this(4);
    }

    /**
       Constructs a collection with the given initial size.
       @param size Initial size of the collection.
     */
    public WriteableShapeCollectionImpl(final int size) {
        shapes = new ArrayList(size);
    }

    /**
       Constructs a collection from another collection.
       @param c A ShapeCollection to copy from.
     */
    public WriteableShapeCollectionImpl(final ShapeCollection c) {
        ShapeCollectionIterator iter = c.iterator();
        while (iter.hasNext()) {
            shapes.add(iter.next());
        }
    }

    public void addShape(final Shape newShape) {
        shapes.add(newShape);
    }

    public ShapeCollectionIterator iterator() {
        return new ShapeCollectionIterator() {
            private Iterator iterator = shapes.iterator();

            public Shape next() {
                return (Shape) iterator.next();
            }

            public boolean hasNext() {
                return iterator.hasNext();
            }
        };
    }

    public WriteableShapeCollectionIterator getWriteableIterator() {
        return new WriteableShapeCollectionIterator() {
            private Iterator iterator = shapes.iterator();

            public Shape next() {
                return (Shape) iterator.next();
            }

            public boolean hasNext() {
                return iterator.hasNext();
            }

            public void remove() {
                iterator.remove();
            }
        };
    }
}
