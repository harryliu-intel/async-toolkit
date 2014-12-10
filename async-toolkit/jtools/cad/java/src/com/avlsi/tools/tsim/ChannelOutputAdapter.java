/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


/**
 * <p> Empty class that implements <code>ChannelOutputListener</code>. </p>
 *
 * <p> A convenience class that implements <code>ChannelOutputListener</code>
 * with empty methods, much like <code>MouseMotionListener</code> and
 * <code>MouseMotionAdapter</code>.
 *
 * @author Patrick Pelletier
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

package com.avlsi.tools.tsim;

public abstract class ChannelOutputAdapter implements ChannelOutputListener {

    // Documented in parent.

    public void sendRequested(ChannelOutput ochan, Message m, long time) {
        // empty
    }

    // Documented in parent.

    public void sendCompleted(ChannelOutput ochan, Message m, long time) {
        // empty
    }

} // end of abstract class ChannelOutputAdapter

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

