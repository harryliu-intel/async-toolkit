// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;

import com.avlsi.fast.shapes.ShapesBlockIterator;
import com.avlsi.fast.shapes.Shape;

/**
   Interface used by clients of JCast or JFast to interact
   with shapes blocks.
   @author Christopher A. Brichford ( chrisb@avlsi.com )
   @see com.avlsi.fast.shapes.ShapesBlockIterator
   @see com.avlsi.fast.shapes.Shape
 */
public interface ShapesBlock {
    ShapeCollection getShapes();
}
