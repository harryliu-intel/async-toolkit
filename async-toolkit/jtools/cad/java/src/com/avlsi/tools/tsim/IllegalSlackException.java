/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/tsim/BufferedChannel.java#51 $
 * $DateTime: 2002/03/06 19:11:52 $
 * $Author: jlm $
 */

package com.avlsi.tools.tsim;

/**
 * <p> Thrown to indicate that a channel has been constructed with an illegal
 * slack value. </p>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class IllegalSlackException extends IllegalArgumentException {

    /**
     * Constructs an <code>IllegalSlackException</code> with no detail
     * message.
     **/
    public IllegalSlackException() {
        super();
    }

    /**
     * Constructs an <code>IllegalSlackException</code> with an argument
     * indicating the illegal slack value.
     *
     * @param n unknown.
     **/
    public IllegalSlackException(final int n) {
        super("Bad slack: " + n);
    }

    /**
     * Constructs an <code>IllegalSlackException</code> with the
     * specified detail message.
     *
     * @param message unknown.
     **/
    public IllegalSlackException(final String message) {
        super(message);
    }

} // end of class IllegalSlackException

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
