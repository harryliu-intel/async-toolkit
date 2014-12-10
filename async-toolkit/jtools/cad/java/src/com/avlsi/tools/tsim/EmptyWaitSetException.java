/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/tsim/Wait.java#17 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */
package com.avlsi.tools.tsim;

/**
 * <p> Exception thrown when attempting to wait on an empty wait set. </p>
 *
 * @see AccurateWait#select
 * @see Wait#select
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date: 2002/02/23 $
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class EmptyWaitSetException extends IllegalStateException {

    /**
     * Constructs an EmptyWaitSetException with no detail message. A detail
     * message is a String that describes this particular exception.
     **/
    public EmptyWaitSetException() {
        super();
    }

    /**
     * Constructs an EmptyWaitSetException with the specified detail
     * message. A detail message is a String that describes this particular
     * exception.
     *
     * @param message the String that contains a detailed message
     **/
    public EmptyWaitSetException(final String message) {
        super(message);
    }

    public EmptyWaitSetException(final String message, final Throwable cause) {
        super(message, cause);
    }
} // end of class EmptyWaitException

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
