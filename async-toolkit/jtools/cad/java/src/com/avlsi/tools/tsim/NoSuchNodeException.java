/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/tsim/BufferedChannel.java#51 $
 * $DateTime: 2002/03/06 19:11:52 $
 * $Author: jlm $
 */

package com.avlsi.tools.tsim;

/**
 * <p> Thrown to indicate that a node needed to construct a channel could not be
 * found. </p>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date: 2002/03/06 $
 *
 * @review kiniry - Under review beginning 29 July 2002. 
 **/

public class NoSuchNodeException extends RuntimeException {

    /**
     * Constructs an <code>NoSuchNodeException</code> with no detail
     * message.
     **/
    public NoSuchNodeException() {
        super();
    }

    /**
     * Constructs an <code>NoSuchNodeException</code> with a detail message
     * indicating the specified node could not be found.
     **/
    public NoSuchNodeException(final String node) {
        super("Can't find node " + node);
    }


} // end of class NoSuchNodeException

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
