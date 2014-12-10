/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

import com.avlsi.fast.shapes.WriteableShapeCollection;
import com.avlsi.fast.shapes.ShapeCollection;

/**
   Interface used by clients of JCast or JFast to interact
   with shapes blocks.  This interface allows the user to modify
   the contents of shapes block.
   @author Christopher A. Brichford ( chrisb@avlsi.com )
   
 */
public interface WriteableShapesBlock extends ShapesBlock{
    /**
       Addes the shapes in the specified collection to the block.
       @param NewShapes Collection of shapes to add.
     */
    void addShapes( ShapeCollection NewShapes );

    /**
       @return The collection of shapes stored in the block.
       If a shape is removed from the collection then the shape will
       be removed from the block.  If a shape is added to the
       collection than the shape will also be added to the block.
     */
    WriteableShapeCollection getWriteableShapes();
}
