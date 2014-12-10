/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.util.debug.Debug;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;
import java.util.LinkedList;
import com.avlsi.tools.dsim.Event;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Node;
import com.avlsi.util.container.Pair;

/**
 * <p> Subclass of <code>Wait</code> that does the same thing, but always
 * returns the earliest message or transition that happened (in simulated time,
 * not real time). </p>
 *
 * <p> You can use this class just like <code>Wait</code>.  However, to be
 * timing-accurate, you need to be careful about actually servicing the first
 * message that comes in.  For example, code like this:
 * <pre>
 * ChannelInput[] inputs = new ChannelInput[] {IO[0].Y, IO[1].Y, IO[2].Y};
 * Wait w  = new Wait(inputs, null);
 *
 * while (true) {
 *    w.select();
 *    if (IO[0].Y.probeReceive()) handleLoadStoreRequest(0);
 *    if (IO[1].Y.probeReceive()) handleLoadStoreRequest(1);
 *    if (IO[2].Y.probeReceive()) handleSend();
 * }
 * </pre>
 * needs to be changed to do this:
 * <pre>
 * ChannelInput[] inputs = new ChannelInput[] {IO[0].Y, IO[1].Y, IO[2].Y};
 * AccurateWait w  = new AccurateWait(inputs, null);
 * 
 * while (true) {
 *    Waitable wable = w.select();
 *    if (wable == IO[0].Y) handleLoadStoreRequest(0);
 *    else if (wable == IO[1].Y) handleLoadStoreRequest(1);
 *    else if (wable == IO[2].Y) handleSend();
 *    else throw new AssertionFailure("not good");
 * }
 * </pre>
 * since multiple channels may be readable, but you need to read from the one
 * with the earliest time first. </p>
 *
 * @author Patrick Pelletier
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public class AccurateWait extends Wait 
{
    // Attributes

    /**
     * This is used to control debugging output.  When it is <code>true</code>,
     * lots of stuff is printed to <code>System.err</code>.  When it is
     * <code>false</code>, lots of stuff is not printed.
     **/

    private static final boolean DEBUG = false || TSimDebug.DEBUG;

    /**
     * The last event that was returned by <code>select()</code>.  This is used
     * to make sure that the caller services this event before calling
     * <code>select()</code> again.
     **/

    private ReadyEvent prev_event = null;

    /**
     * These are instances of <code>ReadyEvent</code> which have already fired.
     * Therefore, they are already in the right order, and all we need to do is
     * deliver them to the caller in order.  We only need to worry about waiting
     * for new events when this list is empty.
     **/

    private final LinkedList/*<ReadyEvent>*/ readyEvents =
        new LinkedList/*<ReadyEvent>*/();

    /**
     * These are instances of <code>ReadyEvent</code> which have been scheduled
     * with the <code>DigitalScheduler</code>, but have not yet fired.  The
     * point of keeping this information is so that we do not schedule them
     * again.
     **/

    private final HashSet/*<ReadyEvent>*/ already_ready =
        new HashSet/*<ReadyEvent>*/();


    // Constructors

    /**
     * Create a new instance of <code>AccurateWait</code>.
     *
     * @param in an array of input channels on which to wait.
     * @param out an array of output channels on which to wait.
     * @param up an array of nodes to wait for going up.
     * @param down an array of nodes to wait for going down.
     *
     * @design Normally, a select() is used to wait on only input channels to
     * determine when there is data ready to be read.  Given that our channels
     * are of finite capacity, it is possible to block during a send.
     *
     * @requires Inherits all contracts of parent.
     **/

    public AccurateWait(ChannelInput [] in, ChannelOutput [] out,
                        Node [] up, Node [] down) {
        super(in, out, up, down);
    }

    // Inherits all documentation and specification of parent.

    public AccurateWait(ChannelInput [] in, ChannelOutput [] out) {
        this(in, out, null, null);
    }


    // Public Methods

    /**
     * Returns the <code>Waitable</code> (from among the <code>Waitables</code>
     * you are waiting on) which was ready first, in simulation time.  This is
     * exactly like the <code>select</code>() method of <code>Wait</code>, except
     * that we return the one which was ready first in simulation time, rather
     * than in real time.
     *
     * @return The <code>Waitable</code> which was ready first, in simulation time
     * order.
     * @exception EmptyWaitSetException when the wait set is empty.
     * @exception InterruptedException when somebody calls
     * <code>Thread.interrupt()</code> on this thread.
     * @exception UnservicedEventException When the event which
     * <code>select()</code> returned last time is still pending and has
     * not been disabled when you call
     * <code>select()</code> again; therefore, you didn't actually service the
     * event. You didn't read a token from the channel like you were supposed to.
     *
     * deprecated  Use {@link #select2} instead.  There is no way of
     *     knowing the time the event happened with this method.
     *
     * <pre><jml>
     * also
     * private exceptional_behavior
     *   requires prev_event != null && prev_event.stillTrue() &&
     *            (ins.contains(prev_event.getWaitable()) ||
     *             outs.contains(prev_event.getWaitable()) ||
     *             ups.contains(prev_event.getWaitable()) ||
     *             downs.contains(prev_event.getWaitable()))
     *   signals (UnservicedEventException uee);
     * also
     * protected exceptional_behavior
     *   requires !nonEmpty();
     *   signals (EmptyWaitSetException ewse);
     * </jml></pre>
     **/

    public synchronized /*@ non_null @*/ Waitable select()
            throws EmptyWaitSetException, InterruptedException,
            UnservicedEventException {
        return (Waitable) select2().getFirst();
    }


    /**
     * Returns the <code>Waitable</code> (from among the <code>Waitables</code>
     * you are waiting on) which was ready first, in simulation time,
     * along with the time at which it became ready.  This is
     * exactly like the <code>select2</code>() method of <code>Wait</code>, except
     * that we return the one which was ready first in simulation time, rather
     * than in real time.
     *
     * @return The <code>Waitable</code> which was ready first, in simulation time
     * order.
     * @exception EmptyWaitSetException when the wait set is empty.
     * @exception InterruptedException when somebody calls
     * <code>Thread.interrupt()</code> on this thread.
     * @exception UnservicedEventException When the event which
     * <code>select()</code> returned last time is still pending and has
     * not been disabled when you call
     * <code>select()</code> again; therefore, you didn't actually service the
     * event. You didn't read a token from the channel like you were supposed to.
     *
     * <pre><jml>
     * also
     * private exceptional_behavior
     *   requires prev_event != null && prev_event.stillTrue() &&
     *            (ins.contains(prev_event.getWaitable()) ||
     *             outs.contains(prev_event.getWaitable()) ||
     *             ups.contains(prev_event.getWaitable()) ||
     *             downs.contains(prev_event.getWaitable()))
     *   signals (UnservicedEventException uee);
     * also
     * protected exceptional_behavior
     *   requires !nonEmpty();
     *   signals (EmptyWaitSetException ewse);
     * </jml></pre>
     **/

    public synchronized /*@ non_null @*/ Pair/*<Waitable,Long>*/ select2()
            throws EmptyWaitSetException, InterruptedException,
            UnservicedEventException {
        // Cached time to compute how much time we spend waiting.
        long timeOfSelectCall;

        // Get the time that this select() was called.
        if (Statistics.STATISTICS)
            timeOfSelectCall = System.currentTimeMillis();
        // We must increment the total select() time when this method returns.

        ReadyEvent event;
	
        if (DEBUG)
            System.err.println("Waitables: " + getWaitables());
        
        // It is illegal to call wait() on a <code>Waitable</code> with no channels.
        if (!nonEmpty())
            throw new EmptyWaitSetException();

        // Make sure we serviced the last event.
        if (prev_event != null && prev_event.stillTrue() &&
            ((prev_event.getType() == ReadyEvent.IN_READY &&
              ins.contains  (prev_event.getWaitable())) ||
             (prev_event.getType() == ReadyEvent.OUT_READY &&
              outs.contains (prev_event.getWaitable())) ||
             (prev_event.getType() == ReadyEvent.UP_READY &&
              ups.contains  (prev_event.getWaitable())) ||
             (prev_event.getType() == ReadyEvent.DOWN_READY &&
              downs.contains(prev_event.getWaitable()))))
            throw new 
                UnservicedEventException("You didn't service event before calling " +
                                         "select() again");

        do {
            if (readyEvents.size() == 0) {
                // If the list of events which have already fired is empty, wait for one
                // to fire.
                try {
                    waiting = true;
                    setWaiters();
                    if (DEBUG)
                        System.err.println("Calling queueEvents()");
                    if (!queueEvents()) {
                        DigitalScheduler.get().decThreadCount();
                        wait();
                        if (DEBUG)
                            System.err.println("Done waiting");
                        Debug.assertTrue(waiting == false);
                    }
                    Debug.assertTrue(readyEvents.size() > 0);
                } finally {
                    clearWaiters();
                    waiting = false;
                }
            }

            // Get the first event off of the list of events which have already fired.
            event = (ReadyEvent) readyEvents.removeFirst();
            if (DEBUG)
                System.err.println("Got event: "+ event);

            // The while condition below is to discard any events which have already
            // been serviced.
      
            // @review Pelletier? - Something to think about: if somebody services an
            // event before we return it, that seems like a symptom that they are
            // doing things out of order.  Maybe we should change this to an
            // assertion, rather than being so permissive?

        } while (!event.stillTrue());

        if (DEBUG)
            System.err.println("Returning event: " + event);
        prev_event = event;

        // Gather statistics on this select call.
        if (Statistics.STATISTICS)
            Statistics.incrementSelectWaitTime(System.currentTimeMillis() -
                                               timeOfSelectCall);

        return new Pair/*<Waitable,Long>*/(event.getWaitable(),
                                           new Long(event.getTime()));
    }

    /**
     * Don't actually wake up; just put new events on the scheduler, which
     * might eventually cause us to wake up.
     **/

    public synchronized void wakeUp() {
        if (DEBUG)
            System.err.println("wakeUp() called");
        if (queueEvents() && waiting) {
            DigitalScheduler.get().incThreadCount();
            waiting = false;
            notify();
        }
    }

    /**
     * Adds the input channels specified to the list of channels to wait for, or
     * enables disabled channels.
     *
     * @param newins the input channels to operate upon.
     **/

    public synchronized void addRead(ChannelInput [] newins) {
        super.addRead(newins);
    }

    /**
     * Adds the input channel specified to the list of channels to wait for, or
     * enable a disabled channel.
     *
     * @param newins the input channel to operate upon.
     **/

    public synchronized void addRead(ChannelInput in) {
        super.addRead(in);
    }

    /**
     * Adds the input channels specified to the list of channels to wait for, or
     * enables disabled channels.
     *
     * @param newouts the output channels to operate upon.
     **/

    public synchronized void addWrite(ChannelOutput [] newouts) {
        super.addWrite(newouts);
    }

    /**
     * Adds the input channel specified to the list of channels to wait for, or
     * enable a disabled channel.
     *
     * @param out the input channel to operate upon.
     **/

    public synchronized void addWrite(ChannelOutput out) {
        super.addWrite(out);
    }

    /**
     * Remove the given channel from the wait set.
     *
     * @param in the input channel to disable.
     **/

    public synchronized void disableRead(ChannelInput in) {
        super.disableRead(in);
    }

    /**
     * Remove the given channel from the wait set.
     *
     * @param out the output channel to disable.
     **/

    public synchronized void disableWrite(ChannelOutput out) {
        super.disableWrite(out);
    }

    /**
     * Removes all the channels which are currently ready.  
     *
     * @review Patrick? - Not sure if that's a wise thing to do, though.
     * @review kiniry - Is anyone calling this?
     **/

    public synchronized void removeReady() {
        super.removeReady();
    }

    // Protected Methods

    /**
     * For <code>AccurateWait</code>, this returns <code>false</code>, so events
     * are dispatched in order.  For <code>InAccurateWait</code>, this is
     * overridden to return <code>true</code>, so events are dispatched randomly.
     **/

    protected /*@ pure @*/ boolean wantRandomEvents() {
        return false;
    }

    // Private Methods

    /**
     * This is called by the <code>fire()</code> method of
     * <code>ReadyEvent</code>.  It does two things:
     * <ol>
     * <li> It adds the event at the end of the linked list of
     * <code>ReadyEvents</code> which have already fired. </li>
     * <li> If a thread is currently waiting in the <code>select()</code> method
     * of this <code>AccurateWait</code> object, wake it up. </li>
     * </ol>
     *
     * @param r the event to handle.
     *
     * <pre><jml>
     * private normal_behavior
     *   requires r != null;
     *   ensures readyEvents.contains(r);
     * </jml></pre>
     **/

    private synchronized void fireEvent(ReadyEvent r) {
        // The event being fired *must* be the earliest event in the entire
        // system.
//         Debug.assertTrue(r.getTime() <= BufferedChannel.youngestEventOnAnyChannel(),
//                          "\nEarliest event fired by AccurateWait has time " + 
//                          r.getTime() + 
//                          ",\nbut the youngest event on any channel has time " +
//                          BufferedChannel.youngestEventOnAnyChannel() + ".");

        readyEvents.add(r);
        if (DEBUG)
            System.err.println("fired: " + r);
        if (waiting) {
            DigitalScheduler.get().incThreadCount();
            waiting = false;
            notify();
            if (DEBUG)
                System.err.println("woke up thread");
        }
    }

    /**
     * Find which channels (or nodes) are ready to be read or written, and
     * schedule <code>ReadyEvents</code> for them on the
     * <code>DigitalScheduler</code> if they have not already been scheduled.
     **/

    private boolean queueEvents() {
        // get any events that are currently ready
        Set/*<ReadyEvent>*/ ready_now = allReady();
        if (DEBUG)
            System.err.println("1) ready_now = " + ready_now);

        // prune already_ready by removing events which are no longer ready
        already_ready.retainAll(ready_now);
        if (DEBUG)
            System.err.println("2) already_ready = " + already_ready);

        // make ready_now be just those events that are ready now but
        // weren't ready before
        ready_now.removeAll(already_ready);
        if (DEBUG)
            System.err.println("3) ready_now = " + ready_now);

        // for next time, add the currently ready events to already_ready
        already_ready.addAll(ready_now);
        if (DEBUG)
            System.err.println("4) already_ready = " + already_ready);

        // now actually queue the new events with the scheduler
        Iterator iready = ready_now.iterator();
        boolean nodeEventAdded = false;
        while (iready.hasNext()) {
            ReadyEvent r = (ReadyEvent) iready.next();
            if (r.getWaitable() instanceof Node) {
                readyEvents.add(r);
                nodeEventAdded = true;
            } else {
                DigitalScheduler.get().addEvent(r);
            }
        }
        return nodeEventAdded;
    }

    /** 
     * @design Only needed for debugging
     *
     * @return the set of all instances of <code>Waitable</code> that are
     * currently contained in <code>ins</code>, <code>outs</code>,
     * <code>ups</code>, and <code>downs</code>.
     **/

    private Set/*<Waitable>*/ getWaitables() {
        Set/*<Waitable>*/ result = new HashSet/*<Waitable>*/();

        result.addAll(ins);
        result.addAll(outs);
        result.addAll(ups);
        result.addAll(downs);

        return result;
    }

    /**
     * Creates an instance of <code>ReadyEvent</code> for every event which is
     * ready.  Each <code>ReadyEvent</code> is given the time at which the event
     * is supposed to happen.  (i.e., the time at which a send or receive on
     * that channel could occur.)
     *
     * @return a set of <code>ReadyEvents</code>, one for each channel (or node)
     * that is ready to be read or written.
     *
     * @bug kiniry 23 July 2002 - Memory allocation and garbage
     * collection abuse caused by data-structure misuse.  Any
     * data-structure that is (a) large, (b) static, and (c) is going
     * to be walked frequently needs to provide a non-interator-based
     * interface.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1164">Bug#1164</a>
     **/

    private Set/*<ReadyEvent>*/ allReady() {
        Set/*<ReadyEvent>*/ result = new HashSet/*<ReadyEvent>*/();

        for (Iterator i = ins.iterator(); i.hasNext();) {
            ChannelInput in = (ChannelInput)i.next();
            if (in.checklessProbeReceive()) {
                result.add(new ReadyEvent(in.getReceiveTime(),
                                          ReadyEvent.IN_READY, in));
            }
        }

        for (Iterator i = outs.iterator(); i.hasNext();) {
            ChannelOutput out = (ChannelOutput)i.next();
            if (out.checklessProbeSend()) {
                result.add(new ReadyEvent(out.getSendTime(),
                                          ReadyEvent.OUT_READY, out));
            }
        }

        for (Iterator i = ups.iterator(); i.hasNext();) {
            Node up = (Node)i.next();
            if (up.getValue() == Node.VALUE_1 ) {
                result.add(new ReadyEvent(up.getTime(),
                                          ReadyEvent.UP_READY, up));
            }
        }

        for (Iterator i = downs.iterator(); i.hasNext();) {
            Node down = (Node)i.next();
            if (down.getValue() == Node.VALUE_0) {
                result.add(new ReadyEvent(down.getTime(),
                                          ReadyEvent.DOWN_READY, down));
            }
        }

        return result;
    }


    // Inner Classes

    /**
     * <code>ReadyEvents</code> are used to signal which events (channel reads or
     * writes) are ready to be handled by <code>DigitalScheduler</code>.
     *
     * @bug kiniry 24 July 2002 - Another sore-point in AccurateWait is
     * AccurateWait$ReadyEvent.  Instances of ReadyEvent, a private inner class in
     * AccurateWait, are used to represent events within the simulation framework.
     * Unfortunately, they are one-offs, created for a single use then tossed, ready
     * for garbage collection.  Within a typical TSim run several million
     * ReadyEvents are created and collected, slowing down the simulator.  If we can
     * find some way to refactor their design and use, this will help out with both
     * kinds of complexity problems in TSim.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1165">Bug#1165</a>
     **/

    private class ReadyEvent implements Event 
    {
        public static final int IN_READY = 0;
        public static final int OUT_READY = 1;
        public static final int UP_READY = 2;
        public static final int DOWN_READY = 3;

        /*@ private 
         @   invariant wable != null;
         @  private
         @   invariant time >= 0;
         @  private
         @   invariant type == IN_READY || type == OUT_READY ||
         @             type == UP_READY || type == DOWN_READY;
         @*/

        /**
         * The <code>Waitable</code> which this event happened on.
         **/

        private final Waitable wable;

        /**
         * The simulation time at which this ReadyEvent should fire.
         **/

        private final long time;

        /**
         * What type of event is this ReadyEvent for?  One of the *_READY constants
         * above.
         **/
    
        private final int type;

        /**
         * This is an opaque integer that we keep track of for the
         * <code>DigitalScheduler</code>.
         **/

        private int index = -1;

        /**
         * Creates a new <code>ReadyEvent</code>.
         *
         * @param time the simulation time at which this <code>ReadyEvent</code>
         * should fire.
         * @param type the type of event (one of *_READY constants).
         * @param wable the <code>Waitable</code> which this event happened on.
         *
         * <pre><jml>
         * normal_behavior
         *   requires time >= 0;
         *   requires type == IN_READY || type == OUT_READY ||
         *            type == UP_READY || type == DOWN_READY;
         *   requires wable != null;
         *   ensures getTime() == time;
         *   ensures getType() == type;
         *   ensures getWaitable() == wable;
         * </jml></pre>
         **/

        ReadyEvent(long time, int type, Waitable wable) {
            this.time = time;
            this.type = type;
            this.wable = wable;
        }

        /**
         * @eturn the simulation time at which this <code>ReadyEvent</code> should
         * fire.
         **/

        public /*@ pure @*/ long getTime() {
            return time;
        }

        /**
         * @return the type of event (one of *_READY constants).
         **/

        public /*@ pure @*/ int getType() {
            return type;
        }

        /**
         * Returns the Waitable which this event happened on
         **/

        public /*@ pure @*/ Waitable getWaitable() {
            return wable;
        }

        /**
         * @reuturn a string representing this <code>ReadyEvent</code> for debugging
         * purposes.
         **/

        public /*@ pure @*/ String toString() {
            StringBuffer buf = new StringBuffer("<ReadyEvent: ");
            switch (type) {
                case IN_READY:
                    buf.append("IN_READY");
                    break;
                case OUT_READY:
                    buf.append("OUT_READY");
                    break;
                case UP_READY:
                    buf.append("UP_READY");
                    break;
                case DOWN_READY:
                    buf.append("DOWN_READY");
                    break;
            }
            buf.append(", ");
            buf.append(time);
            buf.append(", {");
            buf.append(wable);
            buf.append("}, ");
            buf.append(index);
            if (stillTrue())
                buf.append(", still true");
            else
                buf.append(", not true");
            buf.append(">");
            return buf.toString();
        }

        /**
         * @param o another <code>ReadyEvent</code> to compare <code>this</code> to.
         * @return <code>true</code> iff this <code>ReadyEvent</code> has the same
         * time, type, and wable as another <code>ReadyEvent</code>.
         *
         * <pre><jml>
         * also
         * normal_behavior
         *   requires o != null;
         * </jml></pre>
         **/

        public boolean equals(Object o) {
            if (o instanceof ReadyEvent)
                return equals((ReadyEvent)o);
            else
                return false;
        }

        /**
         * @param o another <code>ReadyEvent</code> to compare <code>this</code> to.
         * @return <code>true</code> iff this <code>ReadyEvent</code> has the same
         * time, type, and wable as another <code>ReadyEvent</code>.
         *
         * <pre><jml>
         * normal_behavior
         *   requires r != null;
         * </jml></pre>
         **/

        public boolean equals(ReadyEvent r) {
            return (time == r.time && type == r.type && wable == r.wable);
        }

        // Documented in parent.

        public int hashCode() {
            return ((int) time + type + wable.hashCode());
        }

        /**
         * Sets the index of this <code>ReadyEvent</code>.  The index is used by
         * <code>DigitalScheduler</code> for its own internal purposes.
         *
         * @param index the new index of this event.
         *
         * <pre><jml>
         * also
         * normal_behavior
         *   requires index >= -1;
         *   ensures getIndex() == index;
         * </jml></pre>
         **/

        public void setIndex(int index) {
            this.index = index;
        }

        /**
         * @return the index of this <code>ReadyEvent</code>.
         **/

        public /*@ pure @*/ int getIndex() {
            return index;
        }

        /**
         * This always returns <code>false</code> for <code>ReadyEvent</code>s
         * associated with an <code>AccurateWait</code>, and it always returns
         * <code>true</code> for <code>ReadyEvent</code>s associated with an
         * <code>InaccurateWait</code>.
         *
         * @return <code>false</code>.
         **/

        public boolean isRandom() {
            return wantRandomEvents();
        }

        /**
         * <code>DigitalScheduler</code> calls this when this
         * <code>ReadyEvent</code>'s time has been reached.  This method adds this
         * <code>ReadyEvent</code> to the <code>AccurateWait</code>'s list of events
         * which have fired.
         **/

        public void fire() {
            fireEvent(this);
        }

        /**
         * @return <code>true</code> iff the condition which caused this
         * <code>ReadyEvent</code> to be created in the first place (i.e., a token
         * being ready to be read from a channel) is still true now.
         **/

        public /*@ pure @*/ boolean stillTrue() {
            switch (type) {
                case IN_READY: {
                    ChannelInput in = (ChannelInput) wable;
                    if (in.checklessProbeReceive() && time == in.getReceiveTime())
                        return true;
                }
                    break;
                case OUT_READY: {
                    ChannelOutput out = (ChannelOutput) wable;
                    if (out.checklessProbeSend() && time == out.getSendTime())
                        return true;
                }
                    break;
                case UP_READY: {
                    Node up = (Node) wable;
                    if (up.getValue() == Node.VALUE_1 && time == up.getTime())
                        return true;
                }
                    break;
                case DOWN_READY: {
                    Node down = (Node) wable;
                    if (down.getValue() == Node.VALUE_0 && time == down.getTime())
                        return true;
                }
                    break;
            }
            return false;
        }
    }

} // end of class AccurateWait

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
