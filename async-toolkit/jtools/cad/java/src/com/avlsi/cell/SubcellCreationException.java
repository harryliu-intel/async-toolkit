/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id: $
 */

package com.avlsi.cell;

import com.avlsi.file.common.HierName;

public class SubcellCreationException extends Exception {
    public SubcellCreationException(String msg, HierName subcellName, String parentType) {
        super("Error trying to create the subcell " + subcellName + " in the cell " + parentType + ": " + msg);
    }
}
