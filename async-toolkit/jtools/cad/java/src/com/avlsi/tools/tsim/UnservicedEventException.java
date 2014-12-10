/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/tsim/Wait.java#17 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */
package com.avlsi.tools.tsim;

/**
 * <p> Exception thrown when the previously returned event has not been
 * serviced. </p>
 *
 * @see AccurateWait#select
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date: 2002/02/23 $
 *
 * @review kiniry - Under review beginning 29 July 2002. 
 **/

public class UnservicedEventException extends IllegalStateException {

    /**
     * Constructs an UnservicedEventException with no detail message. A detail
     * message is a String that describes this particular exception.
     **/
    public UnservicedEventException() {
        super();
    }

    /**
     * Constructs an UnservicedEventException with the specified detail
     * message. A detail message is a String that describes this particular
     * exception.
     *
     * @param message the String that contains a detailed message
     **/
    public UnservicedEventException(final String message) {
        super(message);
    }

} // end of class UnservicedEventException

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
