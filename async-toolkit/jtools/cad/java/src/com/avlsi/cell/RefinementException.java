/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cell;

public class RefinementException extends Exception {
    public RefinementException(String msg, String childType, String parentType) {
        super("Error trying to refine " + childType + " from " + parentType +
              ": " + msg);
    }

    public RefinementException(String msg, String childType, String parentType, Throwable cause) {
        super("Error trying to refine " + childType + " from " + parentType +
              ": " + msg, cause);
    }
}
