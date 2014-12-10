/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.util.debug.Debug;

/**
 * <p> Timing handling for <code>BufferedChannel</code>. </p>
 *
 * <p> The following is distilled from "TSim Architecture Overview". </p>
 *
 * @todo Document what a "hole" is.
 *
 * <p> On injection messages propagate as far to the output end as possible.  At
 * the input end they must be separated by at least the cycle time of this
 * channel.</p>
 *
 * <p> On output we dequeue, determine time of dequeue, change message time to
 * that and propagate a "hole" backwards. </p>
 *
 * <p> We maintain one time per slot, but sometimes it is a forward latency
 * transition, and sometimes backwards latency transition.  It always represents
 * "the earliest time a message can arrive." </p>
 *
 * @author Dan Daly
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 22 July 2002.
 *
 * @bug dandaly - FIXME: the handling of latencies is probably wrong.
 **/

class TimingBuffer 
{
    /** Local static debug and verbose flags. **/

    private static final boolean DEBUG = false || TSimDebug.DEBUG;
    private static final boolean VERBOSE = false || TSimDebug.VERBOSE;

    /**
     * <p> List of messages in the channel.   Imagine the left end of this
     * list as the sending side of the channel, and the right end as the
     * receiving side.  Messages are "pushed" as far right as possible
     * when they are added to the channel.  This leads to the first non-trivial
     * invariant on messages that says if a message element is null, so is the
     * element to the left of it. </p>
     *
     * <p> Also, because each messages in the timing buffer is in the same
     * channel, then each message's timestamp must be monotonically increasing
     * when reading from left-to-right (later to earlier messages).  As stated
     * in the second non-trivial invariant below, if two adjacent message slots
     * are non-null, then their times are increasing when read from left to
     * right. </p>
     *
     * <p> To make sure these invariants are non-vacuous, we add guards on the
     * length of the message buffer. </p>
     *
     * <pre><jml>
     * private 
     *   invariant messages != null;
     * private 
     *   invariant messages.length > 1 ==>
     *     (messages[0] == null ==>
     *      (\forall int i; 1 <= i && i <= messages.length - 1;
     *                      messages[i] == null ==> messages[i-1] == null)) ||
     *     (messages[0] != null ==>
     *      (\forall int i; 1 <= i && i <= messages.length - 1;
     *                      messages[i] == null ==> messages[i-1] == null));
     * private 
     *   invariant messages.length > 1 ==>
     *     (messages[0] == null ==>
     *      (\forall int i; 0 <= i && i <= messages.length - 2;
     *                      messages[i] != null ==> messages[i+1] != null)) ||
     *     (messages[0] != null ==>
     *      (\forall int i; 0 <= i && i <= messages.length - 2;
     *                     messages[i] != null ==> messages[i+1] != null));
     * private 
     *   invariant messages.length > 1 ==>
     *     (\forall int i; 0 <= i && i <= messages.length - 2;
     *                     (messages[i] != null && messages[i+1] != null) ==>
     *                     (messages[i].getTime() > messages[i+1].getTime()));
     * </jml></pre>
     **/

    private final Message [] messages;

    /**
     * <p> Corresponding list of times. This array amalgamates </p>
     *
     * <P> If a given slot <var>i</var> of <var>messages</var> references a
     * message instance, then <var>times[i]</var> indicates the earliest time
     * that the corresponding buffer can send the message on to the next
     * buffer. </p>
     *
     * <p> Alternatively, if the slot is <var>null</var>, then the corresponding
     * slot of <var>times</var> indicates the earliest time that the buffer can
     * accept another message. </p>
     *
     * <p> This means that the times of null-related slots must be monotonically
     * increasing from right-to-left (early to late), and each increment must be
     * exactly the forward latency of the buffers, <var>ffLatency</var>. </p>
     *
     * <pre><jml>
     * private invariant messages.length == times.length;
     * private invariant times.length == size;
     * </jml></pre>
     **/

    private final long [] times;
  
    /**
     * The size of the messages and times arrays.
     *
     * <pre><jml>
     * private invariant size >= 1; 
     * </jml></pre>
     **/

    private final int size;
  
    /**
     * Latency constants defined by the associated channel.
     *
     * @see BufferedChannel#ffLatency
     *
     * <pre><jml>
     * private invariant ffLatency > 0;
     * </jml></pre>
     **/

    private final int ffLatency;

    /**
     * The cycle time of the entire channel.  This value is computed
     * from latency constants.
     *
     * @see What for more information?
     **/

    private final long cycleTime;

    /**
     * Cycle time limits for beginning/end of the channel
     * (by default equal to cycleTime).
     **/
    private final long cycleTimeIn;
    private final long cycleTimeOut;

    /**
     * Times that mimic the <code>AbstractDevice</code>s at the ends of
     * the channel.  Each represents the next time that particular end
     * can send or receive, respectively.
     *
     * These values can only grow monotonically.
     **/

    private long sendTime = 0;
    private long receiveTime = 0;
    
    /** 
     * The actual number of messages in the <code>messages</code> array.
     *
     * <p> Access must be done while monitor for <code>this</code> is held.
     *
     * <pre><jml>
     * private invariant (0 <= count) && (count <= size);
     * </jml></pre>
     **/

    private int count = 0;

    /**
     * A name for this TimingBuffer; used for monitoring only.
     **/

    private String name;

    /**
     * Create a new TimingBuffer based upon the provided specification.
     *
     * @param name the name of the timing buffer.  Used for debugging only.
     * @param slack the slack of the associated buffer.
     * @param ffLatency time it takes for a message to get through one slack
     * worth of the buffer.
     * @param cycleTime  The cycle time of the channel.
     * @param cycleTimeIn  The cycle time limit at the beginning of the channel.
     * @param cycleTimeOut  The cycle time limit at the end of the channel.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     *   requires ffLatency >= 0;
     *   requires cycleTime >= ffLatency;
     * </jml></pre>
     **/
    public TimingBuffer(String name, int slack, int ffLatency, int cycleTime,
                        int cycleTimeIn, int cycleTimeOut) 
    {
        this.name = name;
        size = slack;
        messages = new Message [size];
        times = new long [size];
        this.ffLatency = ffLatency;
        this.cycleTime = cycleTime;
        this.cycleTimeIn  = cycleTimeIn;
        this.cycleTimeOut = cycleTimeOut;

        if (DEBUG) {
            Debug.log("instantiated TimingBuffer: " + toString());
        }
    }

    /**
     * @return the earliest time at which the next send can take place.
     **/

    public /*@ pure @*/ synchronized long getSendTime() {
        return Math.max(sendTime, times[0]);
    }

    /**
     * @return the earliest time at which the next receive can take place.
     **/

    public /*@ pure @*/ synchronized long getReceiveTime() {
        return Math.max(receiveTime, times[size-1]);
    }

    /**
     * @return the cycle time of this TimingBuffer, as specified during
     * construction.
     *
     * <pre><jml>
     * public normal_behavior
     *   ensures \result > 0;
     * </jml></pre>
     **/

    public /*@ pure @*/ synchronized long getCycleTime() {
        return cycleTime;
    }

    /**
     * @return whether this buffer is full.  A full buffer cannot
     * enqueue any new messages.
     * @design Note that this class is not thread-safe, so the return
     * value of this method only indicates whether the buffer was full
     * at one time in the past.
     **/

    public /*@ pure @*/ synchronized boolean full() {
        return count == size;
    }

    /**
     * @return the number of messages in this buffer.
     * @design Note that this class is not thread-safe, so the return
     * value of this method only indicates a count that was once
     * correct.
     *
     * <pre><jml>
     * private normal_behavior
     *   ensures \result <= size;
     * </jml></pre>
     **/

    /*@ pure @*/ synchronized int getCount() {
        return count;
    }

    /**    
     * @return whether this buffer is empty.
     * @design Note that this class is not thread-safe, so the return
     * value of this method only indicates whether the buffer was empty
     * at one time in the past.
     **/

    public synchronized /*@ pure @*/ boolean empty() {
        return count == 0;
    }

    public synchronized boolean probeSend(long time) 
    {
        return (!full() && (time >= sendTime));
    }

    public synchronized boolean probeReceive(long time)
    {
        return (!empty() && (time >= receiveTime));
    }
    
    /**
     * Enqueue the message 'm' from a device whose local time was 'deviceTime'
     * on a channel.
     *
     * @design The time of the message 'm' is not ignored.
     *
     * @param m the message to enqueue.
     * @param deviceTime the time of the device sending the message.
     * @return the new time of the sending device.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires m != null;
     *   requires deviceTime >= 0;
     *   ensures \result >= 0;
     * also
     * private normal_behavior
     *   ensures sendTime > \old(sendTime);
     *   assignable sendTime, times, messages, count;
     * </jml></pre>
     *
     * @review kiniry - What should the ensures clause on sendTime actually be?
     * @review kiniry - What should the ensures clause on \result actually be?
     **/

    public synchronized long enqueue(Message m, long deviceTime) {
        long newDeviceTime = Math.max(Math.max(m.getTime(), deviceTime),
                                      Math.max(sendTime, times[0]));
        if (DEBUG) {
            invariant();
            Debug.assertTrue(messages[0] == null);
            Debug.assertTrue(count < size);
            Debug.assertTrue(m != null);
        }
        if (VERBOSE && DEBUG) {
            Debug.log("Before ENQUEUING message '" + m
                      + "', timing buffer =\n" + toString());
        }

        count++;
        sendTime = newDeviceTime + cycleTimeIn;
        times[0] = newDeviceTime;
        messages[0] = m;

        if (VERBOSE && DEBUG) {
            Debug.log("After enqueuing, but before propagating,"
                      + " timing buffer =\n" + toString());
        }

        propagateMessages();

        if (DEBUG) {
            Debug.log("After enqueuing message " + m
                      + ", timing buffer =\n" + toString());
            invariant();
        }

        return newDeviceTime;
    }

    /**
     * @todo Undocumented.
     *
     * @param deviceTime the time of the device sending the message.
     * @return the dequeued message in question.
     *
     * <pre><jml>
     * public normal_behavior
     *  requires deviceTime >= 0;
     *  ensures \result != null;
     * also
     * private normal_behavior
     *  ensures receiveTime > \old(receiveTime);
     *  assignable receiveTime, times, messages, count;
     * </jml></pre>
     *
     * @review kiniry - What should the ensures clause on receiveTime actually be?
     **/

    public synchronized Message dequeue(long deviceTime) {
        long newDeviceTime = Math.max(Math.max(deviceTime, receiveTime),
                                      times[size-1] + ffLatency);
        if (DEBUG) {
            Debug.log("dequeue:"
                      + " deviceTime " + deviceTime
                      + " receiveTime " + receiveTime
                      + " newDeviceTime " + newDeviceTime);

            invariant();
            Debug.assertNotNull(messages[size-1]);
            Debug.assertTrue(count > 0);
        }
        if (VERBOSE && DEBUG) {
            Debug.log("Before DEQUEUING a message, timing buffer =\n" +
                      toString());
        }

        Message rv = messages[size-1];
        rv.setTime(newDeviceTime);

        receiveTime   = newDeviceTime + cycleTimeOut;
        times[size-1] = newDeviceTime + (cycleTimeOut - ffLatency);
        messages[size-1] = null;
        count--;

        if (VERBOSE && DEBUG) {
            Debug.log("After dequeuing, but before propagating,"
                      + " timing buffer =\n" + toString());
        }

        propagateHoles();

        if (DEBUG) {
            Debug.log("After dequeuing message " + rv
                      + ", timing buffer =\n" + toString());
            invariant();
        }

        return rv;
    }
    
    /** 
     * @return if the queue is empty, return a null; otherwise, return a
     * clone of the message that is the next one to be delivered.
     *
     * // @design Why clone like this:
     * //   messages[size-1].clone(times[size-1]);
     * // instead of
     * //   messages[size-1].clone();
     * // or
     * //   messages[size-1].cloneMessage();
     * // If times[size-1] really does matter, then the invariant relating
     * // times and messages is erroneous.
     **/

    public synchronized Message peek() {
        Message result = null;

        if (count == 0)
            return null;
        if (messages[size-1] == null)
            throw new NullPointerException("message is null");
        result = messages[size-1].clone(receiveTime);

        return result;
    }

    // Package Methods

    // Debugging-related
    
    synchronized Message [] debugGetMessageArray() {
        Message[] copy = new Message[messages.length];
        System.arraycopy(messages, 0, copy, 0, messages.length);
        return copy;
    }
    
    synchronized long [] debugGetTimeArray() {
        long[] copy = new long[times.length];
        System.arraycopy(times, 0, copy, 0, times.length);
        return copy;
    }

    // Private Methods
    
    /** 
     * "Propagate" holes in <code>messages</code> and <code>times</code>.
     **/

    private synchronized void propagateHoles() {
        for (int i = size - 2; i >= 0; i--) {
            propagate1(i);
        }
        updateEndTimes();
    }
    
    /** 
     * "Propagate" messages in <code>messages</code> and <code>times</code>.
     **/

    private synchronized void propagateMessages() {
        for (int i = 0; i < size-1; i++) {
            propagate1(i);
        }
        updateEndTimes();
    }

    private synchronized void updateEndTimes() {
        sendTime = Math.max(sendTime, times[0]);
        receiveTime = Math.max(receiveTime, times[size-1]+ffLatency);
    }

    /** 
     * @return a string representation of this timing buffer for debugging
     * purposes.  The send time and receive time of the two ends of the channel
     * are shown, as well as the values of the <code>times</code> list, from
     * sender to receiver.
     **/

    public synchronized String toString() {
        invariant();

        String result;
        // General summary information first...
        result = "TimingBuffer '" + name
            + "': (slack " + size
            + ", ffLatency " + ffLatency
            + ", cycleTime " + cycleTime
            + ", cycleTimeIn " + cycleTimeIn
            + ", cycleTimeOut " + cycleTimeOut + ")\n";
      
        // the channel times first...
        result = result + "\t";
        for (int i = 0; i < size; i++) {
            result = result + "\t" + times[i];
        }
        result = result + "\t[st: " + sendTime + ", rt: " + receiveTime + "]\n\t";

        // then messages
        for (int i = 0; i < size; i++) {
            if (messages[i] != null)
                result = result + "\t" + messages[i].toString();
            else
                result = result + "\tnull";
        }
        result = result + "\n";

        invariant();
        
        return result;
    }
    
    /**
     * Propagate something...?
     * @todo Document.
     *
     * @param m unknown.
     *
     * @review dandaly - if there are timing problems, they're most likely
     * to be here.
     *
     * @requires some relationship between m and size
     * @ensures some relationship between messages and \old(messages)
     * @ensures some relationship between times and \old(times)
     **/ 

    private synchronized void propagate1(int m) {
        if (messages[m] == null) return;
        if (messages[m+1] != null) return;
        long mtime = Math.max(times[m] + ffLatency, times[m+1]);
        long htime = mtime + cycleTime - ffLatency;
        times[m] = htime;
        times[m+1] = mtime;
        messages[m+1] = messages[m];
        messages[m] = null;
    }
    
    /**
     * @invariant spaces before index (size - count) are holes (nulls);
     * after that index, they are messages.
     **/

    private synchronized boolean invariant() {
        boolean result = true;
    
        result = result && (0 <= count && count <= size);
        for (int i = 0; i < size - count; i++) {
            result = result && (messages[i] == null);
        }
        for (int i = size - count; i < size; i++) {
            result = result && (messages[i] != null);
        }
        if (messages.length > 1)
            for (int i = 0; i < messages.length - 1; i++) {
                // equivalent to:
                //  invariant messages.length > 1 ==>
                //    (\forall int i; 0 <= i && i < messages.length - 1;
                //                    messages[i] != null ==> messages[i+1] != null);
                result = result && 
                    (!(messages[i+1] != null) && (messages[i] != null));
            }
        return result;
    }

} // end of class TimingBuffer

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
