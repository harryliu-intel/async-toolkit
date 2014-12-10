/*
 * Copyright 2000 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

/** 
 * <p> This interface sets up a set of callbacks so that another object can
 * monitor the data crossing over the channel.  This is a nice method for
 * implementing logging, and monitoring. </p>
 *
 * @author Dan Daly
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 26 July 2002.
 **/

public interface ChannelInputListener {

    /**
     * A device has begun receiving on this channel.
     *
     * @param ichan The channel where the send is.
     * @param time The time when the send began.
     **/

    /*@ public normal_behavior
      @   requires ichan != null;
      @   requires time >= 0;
      @*/

    void receiveRequested(ChannelInput ichan, long time);

    /**
     * The message has made it through the <code>BufferedChannel</code>.
     *
     * @param ichan The channel where the send is.
     * @param m The message that was received.
     * @param time The time when the send completed.
     **/

    /*@ public normal_behavior
      @   requires ichan != null;
      @   requires m != null;
      @   requires time >= 0;
      @*/

    void receiveCompleted(ChannelInput ichan, Message m, long time);

} // end of interface ChannelInputListener

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
