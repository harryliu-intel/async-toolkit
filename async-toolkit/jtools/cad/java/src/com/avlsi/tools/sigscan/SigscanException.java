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

package com.avlsi.tools.sigscan;

/**
 * Class for throwing Sigscan exceptions
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class SigscanException extends Exception {
    /**
     * Constructor.
     **/
    public SigscanException(String msg) {
        super(msg);
    }

    public SigscanException(String msg, Throwable cause) {
        super(msg, cause);
    }
}

