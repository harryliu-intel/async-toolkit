/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

/**
 * <p> An output channel. </p>
 *
 * @history Modified from <code>HChannelOutput</code> for the new TSim timing
 * model---no more acks.
 *
 * @author Aaron Denney
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 26 July 2002.
 **/

public interface ChannelOutput extends Waitable {

    /** 
     * Peeks at the time on the next message to be sent.
     **/

    /*@ public normal_behavior
      @   ensures \result >= 0;
      @*/

    long getSendTime();

    /** 
     * @return get the cycle time of this particular channel. 
     **/

    /*@ public normal_behavior
      @   ensures \result >= 0;
      @*/

    long getCycleTime();

    /**
     * Get the number of different values this channel can transmit.  If it's an
     * even power of 2, it's equal to 2^bitwidth.  If the channel can transmit
     * any arbitrary <code>BigInteger</code>, this value is -1.
     **/

    /*@ public normal_behavior
      @   ensures \result != null;
      @*/

    BigInteger getNumPossibleValues();

    /**
     * Send a message; blocks until other end receives.
     *
     * @param m the message to enqueue.
     * @param deviceTime the time of the device sending the message.
     * @return the new time of the sending device.
     **/

    /*@
     @ public normal_behavior
     @   requires m != null;
     @   requires deviceTime >= 0;
     @ also
     @ exceptional_behavior
     @   signals (InterruptedException) true;
     @*/

    long send(Message m, long deviceTime) throws InterruptedException;

    
    /**
     * If this returns true, then a send() at deviceTime=time will complete
     * immediately.  If it returns false, and an AccurateWait has been
     * performed at that time, then a send() will not complete immediately.
     * Otherwise the result of this method is arbitrary.
     **/
    boolean probeSend(long time);
    boolean checklessProbeSend(long time);

    /**
     * Can we send without blocking?.  After calling this method, a device
     * should update its time using the result of {@link #getSendTime()}.
     *
     * @modifies QUERY
     * @design Should now be entirely accurate and only return <code>true</code>
     * if the full send cycle can be completed.
     *
     * deprecated  Does not take the device's current time into
     *     consideration, and can return true if the send can
     *     only happen in the future.  Use {@link #probeSend(long)}
     *     instead.
     **/

    /*@
      @ pure
      @*/

    boolean probeSend();

    /**
     * Can we send without blocking?
     *
     * @design Should now be entirely accurate and only return <code>true</code>
     * if the full send cycle can be completed.  Does <strong>NOT</strong> check
     * that the thread on the end of the channel.
     *
     * @bug What does this last sentence mean?
     * @bug Should this method be public at all?
     *
     * deprecated  Does not take the device's current time into
     *     consideration, and can return true if the send can
     *     only happen in the future.  Use {@link #checklessProbeSend(long)}
     *     instead.
     **/

    /*@
      @ pure
      @*/

    boolean checklessProbeSend();

    /**
     * Set an object used for waiting in <code>select()</code>.  
     *
     * @design Public only due to the nature of interfaces.  Should only be used
     * by <code>WaiterInterfaces</code>.
     *
     * @see WaiterInterface
     **/

    /*@ public normal_behavior
      @   requires waiter != null;
      @*/

    void setWriteWaiter(WaiterInterface waiter);

    /**
     * Clear an object used for waiting in <code>select()</code>.  
     *
     * @design Public only due to the nature of interfaces.  Should only be used
     * by <code>WaiterInterfaces</code>.
     *
     * @see WaiterInterface
     **/

    void clearWriteWaiter();

    /**
     * Sets the name of this channel.
     *
     * @param name the new name.
     **/

    /*@ public normal_behavior
      @   requires name != null;
      @   ensures getName().equals(name);
      @*/

    void setName(String name);

    /**
     * @return the name of this channel.
     **/

    /*@
     @ pure
     @*/

    String getName();

    /**
     * Makes this channel unusable.  
     *
     * @design Should also try to make it GCable, and disable any interactions
     * it might have with other objects.
     **/

    void destroy();

} // end of interface ChannelOutput

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
