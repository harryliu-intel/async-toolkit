/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

/**
 * <p> Input channel for <code>Message</code>s. </p>
 *
 * @design Modified from <code>HChannelInput</code> for the new TSim timing
 * model---no more acks.
 *
 * @author Aaron Denney
 * @version $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public interface ChannelInput extends Waitable {

    /** Peeks at the time on the next message to be received **/
    long getReceiveTime();
    
    /** Get the cycle time of this particular channel. **/
    long getCycleTime();

    /**
     * Get the number of different values this channel can transmit.
     * If it's an even power of 2, it's equal to 2^bitwidth.  If the
     * channel can transmit any arbitrary BigInteger, this value is
     * -1.
     **/
    BigInteger getNumPossibleValues();

    /**
     * Wait for a Message on the input channel.
     *
     * Blocks unless ready, or one was latched.
     **/
    Message receive(long deviceTime) throws InterruptedException;

    /**
     * deprecated  Replaced by receive(long)
     **/
    Message receive() throws InterruptedException;

    /**
     * Is there a message waiting?  What is it?  After calling this method,
     * a device should update its time using the result of
     * {@link #getReceiveTime()}.
     *
     * @return <code>null</code> on no message, and the waiting {@link Message}
     * if one exists.
     **/
    Message probeValue();

    /**
     * If this returns true, then a receive() at deviceTime=time will complete
     * immediately.  If it returns false, and an AccurateWait has been
     * performed at that time, then a receive() will not complete immediately.
     * Otherwise the result of this method is arbitrary.
     *
     **/
    boolean probeReceive(long time);
    boolean checklessProbeReceive(long time);

    /**
     * Is there a message waiting?  Should only return true if the
     * full cycle can be completed.  
     *
     * @return <code>false</code> on no message, <code>true</code> if there is one.
     *
     * deprecated  Does not take the device's current time into
     *     consideration, and can return true if the receive can
     *     only happen in the future.  Use {@link #probeReceive(long)}
     *     instead.
     **/
    boolean probeReceive();

    /**
     * Is there a message waiting?  Should only return true if the
     * full cycle can be completed.  <b>Does not check the thread on the channels</b>
     *
     * @return <code>false</code> on no message, <code>true</code> if there is one.
     *
     * deprecated  Does not take the device's current time into
     *     consideration, and can return true if the receive can
     *     only happen in the future.  Use
     *     {@link #checklessProbeReceive(long)} instead.
     **/
    boolean checklessProbeReceive();
    
    /**
     * Sleep until a message is waiting.  After calling this method,
     * a device should update its time using the result of
     * {@link #getReceiveTime()}.
     **/
    void waitForMessage() throws InterruptedException;

    /**
     * Set an object used for waiting in select().  Public only due to the
     * nature of interfaces.  Should only be used by WaiterInterfaces.
     *
     * @see WaiterInterface
     **/
    void setReadWaiter(WaiterInterface waiter);

    /**
     * Clear an object used for waiting in select().  Public only due to the
     * nature of interfaces.  Should only be used by WaiterInterfaces.
     *
     * @see WaiterInterface
     **/
    void clearReadWaiter();

    void setName(String name);
    String getName();

    /**
     * Makes this channel unusable.  Should also try to make it GCable,
     * and disable any interactions it might have with other objects.
     **/
    void destroy();

} // end of interface ChannelInput

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
