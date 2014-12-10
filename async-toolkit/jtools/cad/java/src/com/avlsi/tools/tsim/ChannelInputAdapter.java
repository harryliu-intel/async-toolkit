/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

/**
 * <p> Empty class that implements <code>ChannelInputListener</code>. </p>
 *
 * <p> A convenience class that implements <code>ChannelInputListener</code>
 * with empty methods, much like <code>MouseMotionListener</code> and
 * <code>MouseMotionAdapter</code>. </p>
 *
 * @author Patrick Pelletier
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public abstract class ChannelInputAdapter implements ChannelInputListener {

    // Documented in parent.

    public void receiveRequested(ChannelInput ichan, long time) {
        // empty
    }

    // Documented in parent.

    public void receiveCompleted(ChannelInput ichan, Message m, long time) {
        // empty
    }

} // end of abstract class ChannelInputAdapter

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
