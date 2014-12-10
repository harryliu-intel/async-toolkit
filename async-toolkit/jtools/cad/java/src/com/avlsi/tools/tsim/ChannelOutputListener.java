/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

/**
 * <p> This interface sets up a set of callbacks so that another object can
 * monitor the data crossing over the channel.  This is a nice method for
 * implementing logging, and transaction logging. </p>
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public interface ChannelOutputListener {

    /**
     * A device has begun sending on this channel.
     * @param ochan The channel of the send.
     * @param m The message being sent.
     * @param time The time at which the send began.
     **/
    void sendRequested(ChannelOutput ochan, Message m, long time);

    /**
     * The message has made it through the <code>BufferedChannel</code>.
     * @param ochan The channel of the send.
     * @param m The message that was sent.
     * @param time The time at which the send completed.
     **/
    void sendCompleted(ChannelOutput ochan, Message m, long time);

} // end of interface ChannelOutputListener

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

