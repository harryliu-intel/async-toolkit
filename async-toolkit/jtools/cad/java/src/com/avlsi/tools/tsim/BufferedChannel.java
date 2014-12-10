/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.DigitalScheduler;

import com.avlsi.util.container.ReverseArray;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

/**
 * <p> A representation of a buffered channel. </p>
 *
 * <p> No threads / processes, just a shared object.  The interfaces are not
 * implemented by just forwarding to the channel.  Instead, both the read and
 * write interface have to care about the other side of the Buffer, taking care
 * to ensure that nothing is left sleeping.  I.e. when R <em>is</em> waiting, W
 * needs to put its message in the output Channel, rather than queue.
 *
 * <p> This also necessitates some tricky locking, but is quite doable.
 * Fortunately, we only ever need to care about one reader, and one writer.  If
 * you try to use multiple readers or writers, things <strong>will</strong>
 * break.
 *
 * <!-- Yes, this is an abuse of <dl>.  -->
 * <dl>
 * <dt>Writer to write, no reader:
 *     <dd>
 *         Not full: enqueues message, leaves.
 *             Full: blocks
 *
 * <dt>Writer to write, reader blocked:
 *     <dd><b>should only happen when empty</b><br>
 *         enqueue message; wake reader.
 *
 * <dt>Writer to write, reader selecting:
 *     <dd><b>should only happen when empty</b><br>
 *         enqueue message; wake reader.
 *
 * <dt>Writer to select, no reader:
 *     <dd>
 *         not full: immediate return.
 *             full: go to sleep on writeWaiter
 *
 * <dt>Writer to select, reader blocked:
 *     <dd>empty, so immediate return.
 *
 * <dt>Reader to read, no writer.
 *     <dd>
 *             Empty: block on this
 *         Not empty: dequeue message from ring buffer.
 *
 * <dt>Reader to read, writer blocked on write.
 *     <dd><b>should only happen when full</b><br>
 *         dequeue message, wake writer
 *
 * <dt>Reader to read, writer blocked on select()
 *     <dd><b>should only happen when full</b><br>
 *         Really don't know.
 *
 * <dt>Reader to select, no writer
 *     <dd>
 *             Empty: block on external select object
 *         Not empty: immediate return
 *
 * <dt>Reader to select, writer blocked.
 *     <dd>return
 *
 *
 * <dt>Reader to select, writer selecting.
 *     <dd>Shouldn't happen.  Error, in both programmers part, and
 *         usage.
 * <dt>Writer to select, reader selecting:
 *     <dd>Shouldn't happen.  Error, in both programmers part, and
 *         usage.
 * </dl>
 *
 * <p> Selecting is done by passing in a <code>Waiter</code> object to each
 * channel, which marks it as being in the "select" state. Acquiring the lock
 * for this <code>Waiter</code> object, checking that none of the channels are
 * ready, and then sleeping, and removing the wait object on wakeup.
 *
 * <p> The other side will check for this <code>Waiter</code> object,
 * synchronize on it, and wait.
 *
 * <p> So, threads wait on either this, for reading and writing, or
 * <code>waitReader</code> and <code>waitWriter</code> for
 * <code>select()</code>.
 *
 * @author Aaron Denney
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @see com.avlsi.tools.tsim.TimingBuffer
 * @todo Could also add a remove, and a clear method if ever needed.
 *
 * @review kiniry - Under review beginning 22 July 2002.
 **/

public class BufferedChannel implements ChannelIO, TSimDebug, Statusable,
                                        ChannelStatus
{
    // Static Attributes

    /**
     * Static class debugging switch.
     **/
    private static final boolean DEBUG = false || TSimDebug.DEBUG;

    /**
     * Static debugging switch for checking thread safety.
     **/
    private static final boolean DEBUG_THREAD_SAFETY = false;

    /** 
     * For debugging: give all channels huge (>1000) slack.
     **/
    private static boolean megaSlack = false;

    /**
     * An <code>ArrayList</code> of every <code>BufferedChannel</code> ever
     * constructed so that we can walk them and get time information.
     **/
//     private static final ArrayList everyBufferedChannel = new ArrayList();


    // Attributes

    /** 
     * Slack of the channel.  Also called "capacity".
     *
     * @todo kiniry - Rename "slack" to "capacity" per Peter's comments on the
     * confusion wrt "slack" in the synchronous community.
     *
     * <pre><jml>
     * public
     *   invariant slack > 0;
     * </jml></pre>
     **/

    public final int slack;

    /**
     * <p> The communication between two devices is modeled by two "wires": the
     * data wire (<var>d</var>) and the enable wire (<var>e</var>). </p>
     *
     * <pre>
     *  ffLatency                     ffLatency
     * |---------->                 |------------>
     * +----------+                 +------------+
     * | Device A |A.d           B.d| Device B   |
     * |          |---------------->|            |
     * | Buffer k |                 | Buffer k+1 |
     * |          |e               e|            |
     * |          |<----------------|            |
     * |          |                 |            |
     * +----------+                 +------------+
     * <----------|                 <------------|
     *  bbLatency                     bbLatency
     * </pre>
     *
     * <p> The latency of a buffer with respect to the data wire, the amount of
     * time it takes between the read of a data line and the write to the next
     * data line, is called <var>ffLatency</var>.  'f' stands for "forward", as
     * the information is moving from forward, always modeled from left-to-right
     * (in diagram, data-structures, etc.). </p>
     *
     * <p> The latency in the enable direction, the amount of time a buffer
     * takes between reading an enable and sending an enable is called
     * <var>bbLatency</var>. </p>
     *
     * <p> Associated with each device is also a "<em>round-trip</em>" latency:
     * the amount it times it takes to process the message on <var>d</var> (or
     * <var>e</var>) and then send a new message on <var>e</var> (consequently,
     * <var>d</var>).  These values are represented by the values
     * <var>fbLatency</var> and <var>bfLatency</var>. </p>
     *
     * <p> <var>fbLatency</var> measures the "forward-backward" latency---the
     * amount of time it takes to send from <var>A.d</var> to <var>B.d</var>,
     * process that message in <var>B</var>, then send the complementary message
     * back on <var>e</var>. </p>
     *
     * <p> Likewise, <var>bfLatency</var> represents the "backward-forward"
     * latency: the amount of time it takes to send from <var>B.e</var> to
     * <var>A.e</var>, process that message in <var>A</var>, then send the
     * complementary message back on <var>d</var>. </p>
     *
     * <p> Consider the next diagram. For a buffer k to send two messages
     * serially on d', k must send on d', wait for enable on e', send an enable
     * on e, and get the new data from d. </p>
     *
     * <pre>
     *                  fbLatency      ffLatency    bfLatency
     *               |------------------+     +------------------|
     *   ffLatency                      |     |                      ffLatency
     * |------------>                 |-+-----+-->                |------------>
     *                                  |     |
     * +------------+                 +-+-----+--+                +------------+
     * |            |d               d| |     |  |d'            d'|            |
     * |            |---------------->| |     |  |--------------->|            |
     * | Buffer k-1 |                 | Buffer k |                | Buffer k+1 |
     * |            |e               e| |     |  |e'            e'|            |
     * |            |<----------------| |     |  |<---------------|            |
     * |            |                 | |     |  |                |            |
     * +------------+                 +-+-----+--+                +------------+
     *                                  |     |
     * <------------|                 <-+-----+--|                <------------|
     *   bbLatency                      |     |                      bbLatency
     *               <------------------+     +------------------>
     *                                  bbLatency
     * </pre>
     *
     * <p> Thus, the sum of these latencies is the cycle time of a buffer.
     * Equivalently, twice the sum of <var>fbLatency</var> added to
     * <var>bfLatency</var> is the cycle time as well.  See the diagram for a
     * visual proof of this invariant. </p>
     *
     * <p> Channels have non-zero slack (by definition).  A channel of slack
     * <var>S</var> is modeled as a <var>BufferedChannel</var> with <var>S</var>
     * "buffers".  The cycle time of a <em>buffer</em> is what is
     * <em>actually</em> represented by these latencies.  It is assumed that the
     * the "endpoint" devices are "fast enough", by design, to be ready to send
     * and receive whenever the channels are ready. </p>
     *
     * <p> Thus, endpoint latency is ignored.  Effectively, it is subsumed by
     * these latencies because, when considering the first or last buffer in the
     * channel, <var>fbLatency</var> and <var>bfLatency</var> account for some
     * latency in the endpoint devices. </p>
     *
     * @design kiniry - Endpoint latency is ignored; is this a design bug?  It
     * is unclear whether this is a correct representation or not.
     *
     * <pre><jml>
     * public invariant ffLatency > 0;
     *        invariant bbLatency > 0;
     *        invariant fbLatency > 0;
     *        invariant bfLatency > 0;
     *        invariant 2*(fbLatency + bfLatency) == 
     *                  ffLatency + bbLatency + fbLatency + bfLatency;
     * </jml></pre>
     **/

    public final int ffLatency;
    public final int bbLatency;
    public final int fbLatency;
    public final int bfLatency;
    

    // Package Attributes

    /**
     * The timing buffer associated with this buffered channel.  The timing
     * buffer is responsible for tracking all time/latency-related aspects of a
     * channel.
     *
     * @design kiniry - This attribute has package-level visibility due to
     * <code>youngestMessageInChannel()</code>.
     * @see #youngestMessageInChannel
     **/

    TimingBuffer timingBuffer;


    // Private Attributes

    /** 
     * Enables dynamic class debugging. 
     **/

    private final int debug;

    /** 
     * Name of this channel.  Only used for debugging.
     **/

    private String name;

    /** 
     * Number of transactions which have occured on channel
     **/

    private int count;

    /**
     * The number of different values this channel can transmit.  If it's an
     * even power of 2, it's equal to 2^bitwidth.  If the channel can transmit
     * any arbitrary <code>BigInteger</code>, this value is -1.  If it's not -1,
     * message values are truncated to be between 0 and
     * <var>numPossibleValues</var>-1, inclusive.
     *
     * @bug kiniry - Should such truncation be flagged/logged somewhere obvious.
     * @bug kiniry - Where is "bitwidth" specified?
     *
     * <pre><jml>
     * private invariant numPossibleValues != null;
     * private invariant numPossibleValues.equals(BigInteger.ONE.negate()) ||
     *                   numPossibleValues.compareTo(BigInteger.ZERO) > 0;
     * </jml></pre>
     **/

    private final BigInteger numPossibleValues;

    /** 
     * Default channel latencies.  Note that the latency invariant specified
     * above is true.
     *
     * @see #ffLatency
     * @see #bbLatency
     * @see #fbLatency
     * @see #bfLatency
     **/

    public static final int
        DEFAULT_FF_LATENCY = 200,
        DEFAULT_BB_LATENCY = 300,
        DEFAULT_FB_LATENCY = 300,
        DEFAULT_BF_LATENCY = 200;

    /** 
     * Debugging flags for logging.
     **/

    public static final int MESSAGE_SEND = 1, MESSAGE_RECEIVE = 2,
        MESSAGES = MESSAGE_SEND | MESSAGE_RECEIVE,
        SELECTS_IN = 4, SELECTS_OUT = 8,
        BLOCK = 0x10, ACK_SEND = 0x20, ACK_RECEIVE = 0x40;

    /** 
     * Flags used to represent the state of each end of the channel.
     **/

    private static final int NONE = 0, BLOCKED = 1, SELECT = 2, DESTROYED = 3,
                             NOTHREAD=4;

    /**
     * State of each end. 
     *
     * <p> Access must be done while monitor for <code>this</code> is held.
     **/

    private int reader = NONE;

    /**
     * @bug kiniry 22 July 2002 - Threadanalyzer detected a read/write
     * race on this variable when testing with a 1/1 test Asoc.  The
     * Asoc thread was reading from writer in checklessReceive() while
     * the Master thread was writing to it in checklessSend().
     *
     * @bug kiniry 22 July 2002 - Ibid: Asoc is reading in
     * checklessReceive() while Master is writing in checklessSend().
     *
     * @bug kiniry 23 July 2002 - Lock-cover race: Asoc writing in
     * checklessSend() and reading in probeSend() while Target is
     * reading in checklessReceive().
     *
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1153">Bug#1153</a>
     **/

    private int writer = NONE;

    /** 
     * Synchronization object for <code>select()</code> for reading on the
     * channel.
     *
     * <p> Accesses must be done while holding monitor for
     * <code>this</code>.
     *
     * @todo I don't completely understand this yet.
     **/

    private WaiterInterface readWaiter = null;

    /** 
     * Synchronization object for <code>select()</code> for writing on the
     * channel.
     *
     * <p> Accesses must be done while holding monitor for
     * <code>this</code>.
     *
     * @todo I don't completely understand this yet.
     **/

    private WaiterInterface writeWaiter = null;

    /**
     * The threads currently performing reads and writes, respectively, on this
     * channel.  These values are only used for correctness checks. Only a
     * single thread should be involved on each end, so if this cached identity
     * changes during channel operations, we fail.
     **/

    private Thread readThread, writeThread;

    /**
     * List of listeners for receives.
     *
     * @see #addChannelInputListener(ChannelInputListener)
     * @design Lists is lazily constructed to save memory.
     * @invariant forall i in inputListeners; 
     *              instanceOf(i) == ChannelInputListener
     **/

    private ArrayList/*<ChannelInputListener>*/ inputListeners = null;

    /**
     * List of listeners for sends.
     *
     * @see #addChannelOutputListener(ChannelOutputListener)
     * @design List is lazily constructed to save memory.
     * @invariant forall i in outputListeners; 
     *              instanceOf(i) == ChannelOutputListener
     **/

    private ArrayList/*<ChannelOutputListener>*/ outputListeners = null;


    private static ArrayList /*<ChannelCreationInterface>*/ creationWatchers = 
        new ArrayList();

    // Constructors

    /**
     * Build a channel with a specific slack.
     *
     * @param slack the positive slack of this channel.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     * also
     * public exceptional_behavior
     *   signals (IllegalSlackException ise) slack < 1;
     * </jml></pre>
     **/

    public BufferedChannel(int slack) {
        this(null, slack);
    }

    /**
     * Build a channel with a slack of one (1).  Channel name, bitwidth, and
     * latency are set to default values.
     *
     * @param name the name of this channel.
     **/
    public BufferedChannel(String name) {
        this(name, 1);
    }

    /**
     * Build a channel with a particular name, slack, and bitwidth.  Channel
     * latency is set to the default values.
     *
     * @param name the name of this channel.
     * @param slack the positive slack of this channel.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     * also
     * public exceptional_behavior
     *   signals (IllegalSlackException ise) slack < 1;
     * </jml></pre>
     **/

    public BufferedChannel(String name, int slack,
                           BigInteger numPossibleValues) {
        this(slack, DEFAULT_FF_LATENCY, DEFAULT_BB_LATENCY,
             DEFAULT_FB_LATENCY, DEFAULT_BF_LATENCY, 0, name,
             numPossibleValues);
    }

    /**
     * Build a channel with a particular name and slack.  Bitwidth and latency
     * are set to the default values.
     *
     * @param name the name of this channel.
     * @param slack the positive slack of this channel.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     * also
     * public exceptional_behavior
     *   signals (IllegalSlackException ise) slack < 1;
     * </jml></pre>
     **/

    public BufferedChannel(String name, int slack) {
        this(slack, DEFAULT_FF_LATENCY, DEFAULT_BB_LATENCY,
             DEFAULT_FB_LATENCY, DEFAULT_BF_LATENCY, 0, name);
    }

    /**
     * Build a channel with a particular name, slack, and latency.  Bitwidth is
     * set to the default value.
     *
     * @param slack the positive slack of this channel.
     * @param ffLatency time it takes for a message to get through one slack
     * worth of the buffer.
     * @param bbLatency time it takes for an enable to get through one slack
     * worth of the buffer.
     * @param fbLatency the "forward-backward" round-trip time; see the class
     * documentation above for details.
     * @param bfLatency the "backward-forward" round-trip time; see the class
     * documentation above for details.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     *   requires ffLatency > 0;
     *   requires bbLatency > 0;
     *   requires fbLatency > 0;
     *   requires bfLatency > 0;
     * also
     * public exceptional_behavior
     *   signals (IllegalSlackException ise) slack < 1;
     * </jml></pre>
     **/

    public BufferedChannel(int slack, int ffLatency, int bbLatency,
                           int fbLatency, int bfLatency) {
        this(slack, ffLatency, bbLatency, fbLatency, bfLatency,
             0, null);
    }

    /**
     * Build a channel with a particular name, slack, and latency.  Bitwidth is
     * set to the default value.
     *
     * @param slack the positive slack of this channel.
     * @param ffLatency time it takes for a message to get through one slack
     * worth of the buffer.
     * @param bbLatency time it takes for an enable to get through one slack
     * worth of the buffer.
     * @param fbLatency the "forward-backward" round-trip time; see the class
     * documentation above for details.
     * @param bfLatency the "backward-forward" round-trip time; see the class
     * documentation above for details.
     * @param debug or of flags of events we want spewed to stdout.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     *   requires ffLatency > 0;
     *   requires bbLatency > 0;
     *   requires fbLatency > 0;
     *   requires bfLatency > 0;
     * also
     * public exceptional_behavior
     *   signals (IllegalSlackException ise) slack < 1;
     * </jml></pre>
     **/

    public BufferedChannel(int slack, int ffLatency, int bbLatency,
                           int fbLatency, int bfLatency,
                           int debug, String name) {
        this(slack, ffLatency, bbLatency, fbLatency, bfLatency,
             debug, name, BigInteger.valueOf(-1));
    }

    /**
     * Build a channel with a particular name, slack, bitwidth, and latency.
     *
     * @param slack the positive slack of this channel.
     * @param ffLatency time it takes for a message to get through one slack
     * worth of the buffer.
     * @param bbLatency time it takes for an enable to get through one slack
     * worth of the buffer.
     * @param fbLatency the "forward-backward" round-trip time; see the class
     * documentation above for details.
     * @param bfLatency the "backward-forward" round-trip time; see the class
     * documentation above for details.
     * @param debug or of flags of events we want spewed to stdout.
     * @param numPossibleValues the number of possible values this channel can
     * carry.  This is a more flexible way to represent bitwidth.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     *   requires ffLatency > 0;
     *   requires bbLatency > 0;
     *   requires fbLatency > 0;
     *   requires bfLatency > 0;
     *   requires numPossibleValues != null;
     *   requires numPossibleValues.equals(BigInteger.ONE.negate()) ||
     *            numPossibleValues.compareTo(BigInteger.ZERO) > 0;
     * also
     * public exceptional_behavior
     *   signals (IllegalSlackException ise) slack < 1;
     * </jml></pre>
     **/
    public BufferedChannel(int slack, int ffLatency, int bbLatency,
                           int fbLatency, int bfLatency,
                           int debug, String name,
                           BigInteger numPossibleValues) {
        this(slack, ffLatency, bbLatency, fbLatency, bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             debug, name, numPossibleValues);
    }

    public BufferedChannel(int slack, int ffLatency, int bbLatency,
                           int fbLatency, int bfLatency,
                           int cycleTimeIn, int cycleTimeOut,
                           int debug, String name,
                           BigInteger numPossibleValues) {
        if (slack <= 0)
            throw new IllegalSlackException("Bad slack: " + slack);
	if (megaSlack) slack = 1024;
        this.slack = slack;
        timingBuffer = new TimingBuffer(name, slack, ffLatency,
                ffLatency + bbLatency + fbLatency + bfLatency,
                cycleTimeIn, cycleTimeOut);
        this.ffLatency = ffLatency;
        this.bbLatency = bbLatency;
        this.fbLatency = fbLatency;
        this.bfLatency = bfLatency;
        this.debug = debug;
        this.name = name;
        this.numPossibleValues = numPossibleValues;
        this.count = 0;

        // Add instance to static table of all instances.
//         everyBufferedChannel.add(this);

        // Running count for statistics.
        if (Statistics.STATISTICS)
            Statistics.incrementChannelCount();
        
        synchronized (creationWatchers) {
            for(int loop=0;loop<creationWatchers.size();loop++) {
                ((ChannelCreationInterface) creationWatchers.get(loop))
                    .newChannel(this);
            }
        }
    }

    /** 
     * Legacy constructor.  
     *
     * @deprecated Use an updated constructor.
     * @todo kiniry - Remove these deprecated constructors.
     **/

    public BufferedChannel(int slack, int latency, int backLatency) {
        this(slack, latency, backLatency, DEFAULT_FB_LATENCY, 
             DEFAULT_BF_LATENCY, 0, null);
    }

    /**
     * Legacy constructor.
     *
     * @deprecated Use an updated constructor.
     * @todo kiniry - Remove these deprecated constructors.
     **/
    public BufferedChannel(int slack, int latency,
			   int backLatency, int debug, String name) {
        this(slack, latency, backLatency, DEFAULT_FB_LATENCY, 
             DEFAULT_BF_LATENCY, debug, name);
    }


    // Static Methods

    /**
     * Determine the age of the youngest message in any channel every
     * constructed in this execution of the system.  Since time on messages in
     * channels is monotonic, the result of this method is guaranteed to be no
     * greater than the <em>actual</em> earliest time, given a context switch
     * just after this method returns, but before the value can be checked,
     * might weaken the claim.
     *
     * @ensures That no message in any channel is younger than the result.
     *
     * @concurrency kiniry - This static method is not yet synchronized with
     * any of the rest of this class's concurrent infrastructure.  In
     * particular, we need to ensure that the state of all BufferedChannel's
     * TimingBuffers cannot change in a manner in which will cause this method
     * to fail.
     *
     * <pre><jml>
     * public normal_behavior
     *   ensures \result >= 0;
     * </jml></pre>
     **/
   
    // comment out the next two methods since their implementation
    // causes a memory leak

    // we are using a dummy here since this method is needed by ASOC.java
    public static synchronized /*@ pure @*/ long youngestMessageInAnyChannel() {
//         long youngestTime = Long.MAX_VALUE;
//         BufferedChannel channel = null;
//         Message message = null;
//         long messageTime;
        
//         for (int i = 0; i < everyBufferedChannel.size(); i++) {
//             channel = (BufferedChannel)everyBufferedChannel.get(i);
//             message = channel.timingBuffer.peek();
//             if (message != null) {
//                 messageTime = message.getTime();
//                 if (messageTime < youngestTime)
//                     youngestTime = messageTime;
//             }
//         }
//         if (youngestTime == Long.MAX_VALUE)
//             youngestTime = 0;
//         return youngestTime;
        return 0;
    }

//     public static synchronized /*@ pure @*/ long youngestEventOnAnyChannel() {
//         long youngestTime = Long.MAX_VALUE;
//         BufferedChannel channel = null;
//         long receiveTime = 0, sendTime = 0;
        
//         for (int i = 0; i < everyBufferedChannel.size(); i++) {
//             channel = (BufferedChannel)everyBufferedChannel.get(i);
//             if (channel.checklessProbeReceive()) {
//                 receiveTime = channel.getReceiveTime();
//                 if (receiveTime < youngestTime)
//                     youngestTime = receiveTime;
//             }
//             if (channel.checklessProbeSend()) {
//                 sendTime = channel.getSendTime();
//                 if (sendTime < youngestTime)
//                     youngestTime = sendTime;
//             }
//         }
//         if (youngestTime == Long.MAX_VALUE)
//             youngestTime = 0;
//         return youngestTime;
//     }

    /**
     * Causes all channels created in the future to have a very large (>1000)
     * slack.  This is used only for debugging to easily check if deadlocks are
     * caused by too little slack.
     **/

    public static void enableMegaSlack() {
	megaSlack = true;
    }

    /**
     * Statically build an array of channels of size <var>count</var>, each with
     * slack <var>slack</var>.
     *
     * @param count the number of channels to create.
     * @param slack the positive slack of these channels.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires slack > 0;
     *   requires count > 0;
     * also
     * public exceptional_behavior
     *   signals (IllegalSlackException ise) slack < 1;
     * </jml></pre>
     **/

    public static BufferedChannel [] array(int count, int slack) {
        BufferedChannel [] rv = new BufferedChannel[count];
        for (int i = 0; i < count; i++) {
            rv[i] = new BufferedChannel(slack);
        }
        return rv;
    }

    /**
     * Statically build an array of channels of size <var>count</var>, each with
     * slack <strong>eight</strong> (8).
     *
     * @param count the number of channels to create.
     * @return the corresponding array of new channels.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires count > 0;
     * </jml></pre>
     **/

    public static BufferedChannel [] array(int count) {
        return array(count, 8);
    }

    /**
     * Statically build an array of channels of size <var>stages.length</var>.
     * Each channel <var>i</var> has slack <var>stages[i]</var>.
     *
     * @param stages a description of the stages of the channels.
     * @return the corresponding array of new channels.
     *
     * @exception IllegalSlackException if slack is nonpositive.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires stages != null;
     *   requires stages.length > 0;
     * also
     * public exceptional_behavior
     *   requires false;
     *   signals (IllegalSlackException ise);
     * </jml></pre>
     **/

    public static BufferedChannel [] array(int [] stages) {
        int count = stages.length;
        BufferedChannel [] rv = new BufferedChannel[count];
        for (int i = 0; i < count; i++) {
            rv[i] = new BufferedChannel(stages[i]);
        }
        return rv;
    }


    // Public Methods

    // =================================================================
    // ChannelInput implementation.

    public long getReceiveTime() {
        return timingBuffer.getReceiveTime();
    }

    public long getCycleTime() {
        return timingBuffer.getCycleTime();
    }

    public BigInteger getNumPossibleValues() {
        return numPossibleValues;
    }

    public Message receive(long deviceTime) throws InterruptedException {
        checkReader();
        return checklessReceive(deviceTime);
    }

    public Message receive() throws InterruptedException {
        return this.receive(0);
    }

    public synchronized Message probeValue() {
        if (reader == DESTROYED) { return null; }
        if (DEBUG)
            Debug.assertTrue(reader == NONE || reader == SELECT || reader == NOTHREAD, 
                             "reader in weird state: " + reader);
        checkReader();
        return timingBuffer.peek();
    }

    /**
     * @concurrency Explicitly not synchronized.  Might deadlock select if it
     * were.
     **/
    public boolean probeReceive(long time) 
    {
        checkReader();
        return timingBuffer.probeReceive(time);
    }
    public boolean checklessProbeReceive(long time) 
    {
        return timingBuffer.probeReceive(time);
    }    

    /**
     * @concurrency Explicitly not synchronized.  Might deadlock select if it
     * were.
     **/
    public boolean probeReceive() {
        checkReader();
        return !timingBuffer.empty();
    }

    public boolean checklessProbeReceive() {
        return !timingBuffer.empty();
    }

    public synchronized void waitForMessage() 
            throws InterruptedException {
        // We set reader to NONE at the end, so it had better start
        // as NONE.  It cannot be BLOCKED or SELECT because then
        // the reader would be blocked on that and couldn't call this
        // method.  It can't be NOTHREAD, because he wouldn't want to
        // block waiting for a message.  It can't be DESTROYED, for
        // obvious reasons.  Hence, it must be NONE.
        assert reader == NONE;

	if (timingBuffer.empty()) {
	    reader = BLOCKED;
	    DigitalScheduler.get().decThreadCount();
	    while (timingBuffer.empty()) {
		wait();
	    }
	}

	reader = NONE;
    }

    /**
     * @todo Undocumented.
     **/

    public synchronized void setReadWaiter(WaiterInterface waiter) {
        if (reader == DESTROYED) return;
        if (DEBUG)
            Debug.assertTrue(reader == NONE , "reader in weird state: " + reader);
        checkReader();
        reader = SELECT;
        readWaiter = waiter;
    }

    /**
     * @todo Undocumented.
     **/

    public synchronized void clearReadWaiter() {
        if (reader == DESTROYED) return;
        if (DEBUG)
            Debug.assertTrue(reader == SELECT, "reader in weird state: " + reader);
        checkReader();
        reader = NONE;
        readWaiter = null;
    }

    public synchronized void setReaderNotThreaded() {
        reader = NOTHREAD;
    }

    public synchronized void setWriterNotThreaded() {
        writer = NOTHREAD;
    }
    public void setName(final String newName) {
	name = newName;
    }

    public String getName() {
        return name;
    }

    public int getCount() {
        return count;
    }

    public void incCount() {
        count++;;
    }

    public synchronized void destroy() {
        if (reader == DESTROYED || writer == DESTROYED) return;
        reader = DESTROYED;
        writer = DESTROYED;
        if (readThread != null) {
            readThread.interrupt();
        }
        if (writeThread != null) {
            writeThread.interrupt();
        }
        readWaiter = writeWaiter = null;
        readThread = writeThread = null;
        name = null; timingBuffer = null;
    }

    // end of ChannelInput implementation.
    // =================================================================


    // =================================================================
    // ChannelOutput implementation.

    // Documented in parent.

    /*@ also
      @ public normal_behavior
      @   requires m != null;
      @   requires deviceTime >= 0;
      @   ensures \result > deviceTime;
      @*/

    public long send(Message m, long deviceTime)
            throws InterruptedException {
        checkWriter();
        return checklessSend(m, deviceTime);
    }
    
    /** 
     * @design Explicitly doesn't verify identity of writing thread.  For
     * <code>NodeReadChannel</code>.
     *
     * @param m the message to enqueue.
     * @param deviceTime the time of the device sending the message.
     * @return the new time of the sending device.
     *
     * @bug kiniry 22 July 2002 - Threadalyzer detected a thread stall
     * here when running TestAsoc with one master and one targets (1/1).
     * The monitor that the Master thread is waiting for is on
     * BufferedChannel.  The Master thread holds no locks.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1144">Bug#1144</a>
     *
     * @bug Should this method be public at all?  No, it shouldn't, but
     * ChannelTwigInterface, a part of F16World, is violating send semantics.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1270">Bug#1270</a>
     *
     * <pre><jml>
     * public normal_behavior
     *   requires m != null;
     *   requires deviceTime >= 0;
     *   ensures \result > deviceTime;
     * </jml></pre>
     **/

    public synchronized long checklessSend(Message m, long deviceTime)
            throws InterruptedException {
        // Cached time to compute how much time we spend waiting.
        long timeOfSendCall;

        // Get the time that this send() was called.
        if (Statistics.STATISTICS)
            timeOfSendCall = System.currentTimeMillis();
        // We must increment the total send() time when this method returns.

        if (writer == DESTROYED) {
            // Gather statistics on this send call.
            if (Statistics.STATISTICS)
                Statistics.incrementSendWaitTime(System.currentTimeMillis() -
                                                 timeOfSendCall);

            return deviceTime; 
        }
        if (DEBUG)
            Debug.assertTrue((writer == NONE) || (writer == NOTHREAD), 
                             "writer in weird state: " + writer);

        // Report to any listeners that a send has been requested.
        notifyOutputListenersRequest(deviceTime, m);
        
        if (timingBuffer.full()) {
            /* Debug.maskPrint(MESSAGE_SEND, debug, "Sender "
             + writeThread + " blocking on write of " + m); */
            writer = BLOCKED;
            // We're going to sleep: decrement count of active readers.
            /* Debug.maskPrint(BLOCKED, debug, "Channel "
             + this + " blocking on full!"); */
            DigitalScheduler.get().decThreadCount();
            while(timingBuffer.full()) {
                wait();
            }
            /* Debug.maskPrint(BLOCKED, debug, "Channel "
             + this + " no longer full!"); */
            // Woke up.  But reader adjusted count for us.
        }

        boolean wasEmpty = timingBuffer.empty();

        // If this channel has a size limit, silently truncate
        // messages which are too large.
        if (numPossibleValues.signum() == 1) {
            final BigInteger truncValue =
                m.getValue().mod(numPossibleValues);
            if (! truncValue.equals(m.getValue()))
                m.setValue(truncValue);
        }

        long timestamp = timingBuffer.enqueue(m, deviceTime);
        if (writer != NOTHREAD) writer = NONE;

        /* Debug.maskPrint(MESSAGE_SEND, debug, "Sender "
         + writeThread + " wrote " + m); */

        // Report to any listeners that a send has completed.
        notifyOutputListenersCompleted(timestamp, m);
        
        if (wasEmpty) {
            if (reader == BLOCKED) {
                Debug.assertTrue(readWaiter == null);
                // Waking up reader.
                DigitalScheduler.get().incThreadCount();
                notify();
            } else if (reader == SELECT) {
                Debug.assertTrue(readWaiter != null);
                // Waking up reader.
                readWaiter.wakeUp();
            } else if (reader == NOTHREAD) {
                //System.out.println("Not incrementing count on send, " + 
                //                   "threadless on " + getName());
            }
        }

        // Gather statistics on this send call.
        if (Statistics.STATISTICS)
            Statistics.incrementSendWaitTime(System.currentTimeMillis() -
                                             timeOfSendCall);

        return timestamp;
    }

    /*@
      @ public normal_behavior
      @   requires m != null;
      @*/

    public void send(Message m) throws InterruptedException {
        this.send(m, 0);
    }

    /**
     * If this returns true, then a send() at deviceTime=time will complete
     * immediately.  If it returns false, and an AccurateWait has been
     * performed at that time, then a send() will not complete immediately.
     * Otherwise the result of this method is arbitrary.
     *
     * Explicitly not synchronized.  Might deadlock select if it were.
     *
     * @bug kiniry 23 July 2002 - Improper access to writer.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1153">Bug#1153</a>
     **/
    public boolean probeSend(long time) 
    {
        if (writer == DESTROYED) { return false; }
        if (DEBUG)
            Debug.assertTrue(writer == NONE || writer == SELECT, 
                             "writer in weird state: " + writer);
        checkWriter();
        return timingBuffer.probeSend(time);
    }

    public boolean checklessProbeSend(long time) 
    {
        return timingBuffer.probeSend(time);
    }    

    /**
     * Explicitly not synchronized.  Might deadlock select if it were.
     *
     * @bug kiniry 23 July 2002 - Improper access to writer.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1153">Bug#1153</a>
     **/
    public boolean probeSend() {
        if (writer == DESTROYED) { return false; }
        if (DEBUG)
            Debug.assertTrue(writer == NONE || writer == SELECT, 
                             "writer in weird state: " + writer);
        checkWriter();
        return !timingBuffer.full();
    }

    public boolean checklessProbeSend() {
        return !timingBuffer.full();
    }

    public synchronized void setWriteWaiter(WaiterInterface waiter) {
        if (writer == DESTROYED) return;
        if (DEBUG)
            Debug.assertTrue(writer == NONE, "writer in weird state: " + writer);
        checkWriter();
        writer = SELECT;
        writeWaiter = waiter;
    }

    public synchronized void clearWriteWaiter() {
        if (writer == DESTROYED) return;
        if (DEBUG)
            Debug.assertTrue(writer == SELECT, "writer in weird state: " + writer);
        checkWriter();
        writer = NONE;
        writeWaiter = null;
    }

    public long getSendTime() {
        return timingBuffer.getSendTime();
    }

    // end of ChannelOutput methods
    // =================================================================

    // Returns null if empty channel.  Explicitly not synchronized.
    public Message checklessProbeValue() {
        return timingBuffer.peek();
    }

    /** 
     * Explicitly doesn't verify that this thread is the usual
     * receiving thread.  For NodeWriteChannel.  
     *
     * @param deviceTime unknown.
     * @return unknown.
     * @exception InterruptedException When?
     **/
    public synchronized Message checklessReceive(long deviceTime)
            throws InterruptedException {
        // Cached time to compute how much time we spend waiting.
        long timeOfReceiveCall;

        // Get the time that this receive() was called.
        if (Statistics.STATISTICS)
            timeOfReceiveCall = System.currentTimeMillis();
        // We must increment the total receive() time when this method returns.

        if (reader == DESTROYED) {
            // Gather statistics on this receive call.
            if (Statistics.STATISTICS)
                Statistics.incrementReceiveWaitTime(System.currentTimeMillis() -
                                                    timeOfReceiveCall);

            return null;
        }
        if (DEBUG)
            Debug.assertTrue((reader == NONE) || (reader == NOTHREAD), 
                             "reader in weird state: " + reader);
        
        // Report to any listeners that we are waiting for a receive.
        notifyInputListenersRequest(deviceTime);
        
        if (timingBuffer.empty()) {
            reader = BLOCKED;
            /* Debug.maskPrint(MESSAGE_RECEIVE, debug, "Receiver "
             + readThread + " blocking"); */
            DigitalScheduler.get().decThreadCount();
            while (timingBuffer.empty()) {
                wait();
            }
        }

        boolean wasFull = timingBuffer.full();
        Message rv = timingBuffer.dequeue(deviceTime);
        /* Debug.maskPrint(MESSAGE_RECEIVE, debug, "Receiver "
         + readThread + " reading " + rv); */


        // Report to any listeners that a message has been received.
        notifyInputListenersCompleted(deviceTime, rv);

        if (wasFull) {
            if (writer == BLOCKED) {
                Debug.assertTrue(writeWaiter == null);
                DigitalScheduler.get().incThreadCount();
                notify();
            } else if (writer == SELECT) {
                Debug.assertTrue(writeWaiter != null);
                writeWaiter.wakeUp();
            } else if (writer == NOTHREAD) {
                //System.out.println("Not incrementing in receive here, non-threaded");
            }
        }

        if (reader != NOTHREAD) reader = NONE;

        // Gather statistics on this receive call.
        if (Statistics.STATISTICS) {
            Statistics.incrementReceiveWaitTime(System.currentTimeMillis() -
                                                timeOfReceiveCall);
            Statistics.incrementMessagesDelivered();
        }

        return rv;
    }

    public String toString() {
        return (writeThread == null ? "none" : writeThread.getName()) +
            " -> " +
            (readThread == null ? "none" : readThread.getName()) +
            (name == null ? "" : " (named '" + name + "')");
            
    }

    // Debugging-related Methods

    public String getState() {
        return timingBuffer.getCount() + "/" + slack;
    }

    public boolean idle() {
        return timingBuffer.getCount() == 0;
    }

    public Message [] debugGetMessageArray() {
        return timingBuffer.debugGetMessageArray();
    }

    public long [] debugGetTimeArray() {
        return timingBuffer.debugGetTimeArray();
    }

    public void debugPrintArrays() {
        System.err.println("debugPrintArrays: for channel " + toString());
        System.err.println("debugPrintArrays: state is " + getState());
        Message[] msgs = debugGetMessageArray();
        for (int i = 0; i < msgs.length; i++) {
            System.err.println("debugPrintArrays: msgs[" + i + "] = " + msgs[i]);
        }
        long[] times = debugGetTimeArray();
        for (int i = 0; i < times.length; i++) {
            System.err.println("debugPrintArrays: times[" + i + "] = " + times[i]);
        }
    }

    public boolean debugIsEmpty() {
        Message[] ms = debugGetMessageArray();

        for (int i = 0; i < ms.length; i++)
            if (ms[i] != null)
                return false;
        return true;
    }

    // Listener handling.

    /**
     * Adds an output listener to this channel.  The listening class will be
     * notified every time a send is requested and completed.
     *
     * @param listener the <code>ChannelOutput</code> listener to add to the
     * list.
     *
     * @bug kinriy - Listeners can only be added, not removed.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires listener != null;
     * also
     * private normal_behavior
     *   ensures outputListeners.contains(listener);
     * </jml></pre>
     **/

    public synchronized void addChannelOutputListener(
                                                      ChannelOutputListener listener) {
        if (outputListeners == null)
            outputListeners = new ArrayList();
        outputListeners.add(listener);
    }

    /**
     * Adds an input listener to this channel.  The listening class will be
     * notified every time a receive is requested and completed.
     *
     * @param listener the <code>ChannelOutput Listener to add to the list.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires listener != null;
     * also
     * private normal_behavior
     *   ensures inputListeners.contains(listener);
     * </jml></pre>
     **/

    public synchronized void addChannelInputListener(
                                                     ChannelInputListener listener) {
        if (inputListeners == null)
            inputListeners = new ArrayList();
        inputListeners.add(listener);
    }

    /** Adds a listener which will be reported to when a new channel is
     * created
     **/
    public static void addCreationWatcher(
            ChannelCreationInterface watcher) {
        synchronized(creationWatchers) {
            creationWatchers.add(watcher);
        }
    }
    // Package Methods

    // Private Methods

    private final synchronized void notifyInputListenersRequest(long deviceTime) {
        if (inputListeners != null) {
            for (Iterator i = inputListeners.iterator(); i.hasNext();) {
                ((ChannelInputListener)i.next())
                    .receiveRequested(this, deviceTime);
            }
        }
    }

    private final synchronized void notifyInputListenersCompleted(long deviceTime,
                                                                  Message rv) {
        if (inputListeners != null) {
            for (Iterator i=inputListeners.iterator();i.hasNext();) {
                ((ChannelInputListener)i.next())
                    .receiveCompleted(this, rv, rv.getTime());
            }
        }
    }

    private final synchronized void notifyOutputListenersRequest(long deviceTime,
                                                                 Message m) {
        if (outputListeners != null) {
            for (Iterator i=outputListeners.iterator();i.hasNext();) {
                ((ChannelOutputListener)i.next())
                    .sendRequested(this, m, m.getTime());
            }
        }
    }

    private final synchronized void notifyOutputListenersCompleted(long timestamp,
                                                                   Message m) {
        if (outputListeners != null) {
            for (Iterator i=outputListeners.iterator();i.hasNext();) {
                ((ChannelOutputListener)i.next())
                    .sendCompleted(this, m, timestamp);
            }
        }
    }
    
    /**
     * Check to see if the thread performing a read() is the legitimate one.
     **/

    private void checkReader() {
        if (readThread == null) {
            readThread = Thread.currentThread();
            return;
        }

        if (readThread != Thread.currentThread()) {
            throw new AssertionFailure(name + ": checkReader() - "
				       + Thread.currentThread()
				       + " is not " + readThread);
        }

    }

    private void checkWriter() {
        if (writeThread == null) {
            writeThread = Thread.currentThread();
            return;
        }

        if (writeThread != Thread.currentThread()) {
            throw new AssertionFailure(name + ": checkWriter() - "
                                       + Thread.currentThread()
                                       + "is not" + writeThread);
        }
    }

    public synchronized void printStatus() {
        System.err.println(this + " " + timingBuffer.getCount() + "/" + slack);
    }

    public synchronized boolean isReaderBlocked() {
	    return reader == BLOCKED || reader == SELECT;
    }

    public synchronized boolean isWriterBlocked() {
        return writer == BLOCKED || writer == SELECT;
    }

    public int getCapacity() {
        return slack;
    }   

    public int getAvailable() {
        return slack - timingBuffer.getCount();
    }   

    public String getVerboseStatus() {
        final Message[] messages = timingBuffer.debugGetMessageArray();
        final StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (Message m : new ReverseArray<Message>(messages)) {
            if (m != null) {
                if (first) {
                    first = false;
                } else {
                    sb.append(" ");
                }
                sb.append(m.toString());
            }
        }
        return sb.toString();
    }
} // end of class BufferedChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
