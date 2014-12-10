/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

/**
 * <p> Interface for objects returned from <code>Wait.select()</code>. </p>
 *
 * <p> Currently just a "tagging" interface. Nothing useful can be put in this
 * class, as some objects must me waited on in different ways. I.e., channels
 * can be waited on as either input or output. </p>
 *
 * @see Wait
 * @see ChannelInput
 * @see ChannelOutput
 * @see com.avlsi.tools.dsim.Node
 *
 * @author Aaron Denney
 * @version $Revision$ $Date$
 **/

public interface Waitable {

    // purposefully empty; just a tagging interface

} // end of interface Waitable

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
