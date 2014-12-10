/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cell;

import com.avlsi.file.common.HierName;

/**
 * Exception throws when an operation is attempted on a subcell that 
 * does not exist.
 *
 * @see CellImpl#inlineSubcell
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class NoSuchSubcellException extends Exception {
    public NoSuchSubcellException(final HierName hn) {
        super(hn.getAspiceString());
    }
}
