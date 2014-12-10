/*
 * Copyright 2002, 2003, 2004 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;
import java.util.HashMap;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Event;
import com.avlsi.util.container.BigIntegerReferenceInterface;

/**
 * Base class for single threaded tsim devices.
 *
 * @author Dan Daly
 * @author Jesse Rosenstock
 * @version $Date$
 **/
public abstract class AbstractSingleThreadedDevice
    implements ChannelInputListener,
               ChannelOutputListener,
               Startable {

    private static final int RESET_STATE = 0;

    protected final String name;

    private final HashMap/*<ChannelInput,RecvVect>*/ blockedReceives;

    private final HashMap/*<ChannelOutput,SendVect>*/ blockedSends;

    private final DigitalScheduler digitalScheduler;

    /**
     * Constructor.  Subclass constructors are responsible for 
     * registering this device with any channels using
     * <code>addChannelOutputListener</code> and
     * <code>addChannelInputListener</code>.
     **/
    public AbstractSingleThreadedDevice(final String name) {
        this.name = name;
        blockedReceives = new HashMap/*<ChannelInput,RecvVect>*/();
        blockedSends = new HashMap/*<ChannelOutput,SendVect>*/();
        digitalScheduler = DigitalScheduler.get();
    }

    public void reset() {
        blockedReceives.clear();
        blockedSends.clear();
    }

    public void start() {
        // Schedule the device to start immediately
        digitalScheduler.addEvent(new SingleThreadedEvent(0, getTime()));
    }

    protected abstract void execute(int state);

    protected final void send(BufferedChannel out,
                           BigInteger value,
                           int nextState) {
        // TODO: move this elsewhere
        out.setWriterNotThreaded();

        if (out.checklessProbeSend()) {
            try {
                final long time = getTime();
                final long nextTime =
                    out.checklessSend(new Message(value, time), time);
                // REVIEW: would it be better to continue on here instead
                // of scheduling more events?
                digitalScheduler.addEvent
                    (new SingleThreadedEvent(nextState, nextTime));
            } catch (InterruptedException e) {
                handleInterruptedException(e);
            }
        } else {
            // The only reason that this should happen is if there are
            // two blocking sends executing in parallel, ie (X?x, X?x).
            // Perhaps a more informative error message would be good in
            // that case.  It could also happen if the code is really
            // badly broken.
            assert !blockedSends.containsKey(out);
            blockedSends.put(out, new SendVect(value, getTime(), nextState));
        }
    }

    protected final void receive(BufferedChannel in,
                              BigIntegerReferenceInterface valueRef,
                              int nextState) {
        // TODO: move this elsewhere
        in.setReaderNotThreaded();

        if (in.checklessProbeReceive()) {
            try {
                final Message m = in.checklessReceive(getTime());
                valueRef.setBigInteger(m.getValue());
                digitalScheduler.addEvent
                    (new SingleThreadedEvent(nextState, m.getTime()));
            } catch (InterruptedException e) {
                handleInterruptedException(e);
            }
        } else {
            // See send() for explanation of this assertion.
            assert !blockedReceives.containsKey(in);
            blockedReceives.put(in, new RecvVect(valueRef,
                                                 getTime(),
                                                 nextState));
        }
    }

    protected boolean probeSend(final /*@ non_null @*/ ChannelOutput out) {
        throw new AssertionError();
    }

    protected boolean probeReceive(final /*@ non_null @*/ ChannelInput in) {
        throw new AssertionError();
    }

    protected Message probeValue(final /*@ non_null @*/ ChannelInput in) {
        throw new AssertionError();
    }


    // implement ChannelInputListener interface

    public void receiveRequested(ChannelInput ichan, long time) {
    }

    public void receiveCompleted(ChannelInput ichan,
                                 Message m,
                                 long time) {
        // A receive has completed on a channel we are listening on.
        // Pop off the message waiting for it and go to the next state.
        final SendVect sender = (SendVect) blockedSends.remove(ichan);

        // We will always get the callback, even if we are not waiting
        // on that channel.  We must just return in that case.
        if (sender == null)
            return;

        try {
            // We should no longer be blocked since a receive has happened
            assert ((BufferedChannel) ichan).checklessProbeSend();
            assert time >= sender.time;
            final long nextTime =
                ((BufferedChannel) ichan).checklessSend(new Message(sender.value, sender.time),
                                    sender.time);
            // enqueue an event to execute the next state at the next time
            digitalScheduler.addEvent
                (new SingleThreadedEvent(sender.nextState, nextTime));
        } catch (InterruptedException e) {
            handleInterruptedException(e);
        }
    }


    // implement ChannelOutputListener interface

    public void sendRequested(ChannelOutput ochan, Message m, long time) {
    }

    public void sendCompleted(ChannelOutput ochan, Message m, long time) {
        // A send has completed on a channel we are listening on.
        // Pop off the message waiting for it and go to the next state.
        final RecvVect recv = (RecvVect) blockedReceives.remove(ochan);

        // See receiveCompleted() for why this could be null.
        if (recv == null)
            return;

        try {
            // We should no longer be blocked since a send has happened
            assert ((BufferedChannel) ochan).checklessProbeReceive();
            assert time >= recv.time;
            final Message message =
                ((BufferedChannel) ochan).checklessReceive(recv.time);
            recv.valueRef.setBigInteger(message.getValue());
            digitalScheduler.addEvent
                (new SingleThreadedEvent(recv.nextState,
                                         message.getTime()));
        } catch (InterruptedException e) {
            handleInterruptedException(e);
        }
    }

    /** Vectorizing objects **/

    private static final class SendVect {
        public final BigInteger value;
        public final long time;
        public final int nextState;

        public SendVect(final BigInteger value,
                        final long time,
                        final int nextState) {
            this.value = value;
            this.time = time;
            this.nextState = nextState;
        }
    }

    private static final class RecvVect {
        public final BigIntegerReferenceInterface valueRef;
        public final long time;
        public final int nextState;

        public RecvVect(final BigIntegerReferenceInterface valueRef,
                        final long time,
                        final int nextState) {
            this.valueRef = valueRef;
            this.time = time;
            this.nextState = nextState;
        }
    }
    /********************************************************************/

    /**
     *
     **/
    private final class SingleThreadedEvent implements Event {
        private final int state;

        private final long time;

        private int index;

        private SingleThreadedEvent(final int state, final long time) {
            this.state = state;
            this.time = time;
            // set the index to -1 to indicate the event is not queued
            this.index = -1;
        }

        public void fire() {
            execute(state);
        }

        public void setIndex(int index) {
            this.index = index;
        }

        public /*@ pure @*/ int getIndex() {
            return index;
        }

        public /*@ pure @*/ long getTime() {
            return time;
        }

        public /*@ pure @*/ boolean isRandom() {
            // REVIEW: Should this depend on the dsim timing mode?
            return false;
        }
    }

    /** Exception Handling **/

    //This should never, EVER happen
    protected void handleInterruptedException(InterruptedException e) {
        System.err.println("Interrupted Exception: "+e.getMessage());
        e.printStackTrace();
        System.exit(1);
    }

    private long getTime() {
        return digitalScheduler.getTime();
    }
}
