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

package com.avlsi.io;

/**
 * Throws by methods in <code>PositionStackReader</code> class to indicate
 * that the position stack is empty.
 *
 * @see PositionStackReader
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class EmptyPositionStackException extends RuntimeException {
    /**
     * Constructs a new <code>EmptyPositionStackException</code> with 
     * <tt>null</tt> as its error message string.
     **/
    public EmptyPositionStackException() {
    }

    public EmptyPositionStackException(Throwable cause) {
        super(cause);
    }
}
