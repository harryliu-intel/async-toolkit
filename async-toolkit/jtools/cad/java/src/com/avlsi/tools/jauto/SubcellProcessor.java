/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import com.avlsi.geometry.BoundingBox;

public interface SubcellProcessor {
    /** Called to process the bounding box of the current cell */
    void process(BoundingBox bBox);
    /** Called to process pins in the current cell */
    void process(String pinName, BoundingBox bBox);
    /** Called to process each subcell in the current cell */
    void process(String cellType, String instanceName, int orientation, BoundingBox bBox);
}
