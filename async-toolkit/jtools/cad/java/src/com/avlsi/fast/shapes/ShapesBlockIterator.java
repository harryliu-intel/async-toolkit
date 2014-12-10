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
   Iterator interface used by the ShapesBlock interface.
   Pretty generic iterator interface.
 */
public interface ShapesBlockIterator {
    /**
       Returns the next shape in the shapes block being iterated.
       @return the next shape in the shapes block being iterated.
     */
    Shape next();
    /**
       Returns true if the shapes block has more shapes.
       @return true if there are more shapes in the shapes block.
     */
    boolean hasNext();
}
