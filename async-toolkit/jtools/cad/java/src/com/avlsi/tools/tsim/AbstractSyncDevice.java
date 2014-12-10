/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.WakeAt;
import com.avlsi.tools.sigscan.Transaction;
import com.avlsi.tools.sigscan.TransactionType;
import com.avlsi.tools.sigscan.LongAttribute;
import com.avlsi.tools.sigscan.AttributeValue;

/**
 * <p> This is a superclass for synchronous devices.  Unlike an asynchronous
 * device, interacting with channels does not change the internal time.  The
 * only way to change the internal time is by calling <code>nextCycle()</code>,
 * which increments the internal time by <code>cycleTime</code> (which you
 * should set to be something appropriate), and also uses the digital scheduler
 * to insure that any events which should have already happened in simulated
 * time have already happened in real time. </p>
 *
 * <p> Because of the different time semantics, <code>send()</code> and
 * <code>receive()</code> also have different semantics.  <code>send()</code>
 * throws a <code>ChannelFullException</code> if the channel is full, and
 * <code>receive()</code> throws a <code>ChannelEmptyException</code> if the
 * channel is empty.  You can catch these exceptions if you want to handle these
 * cases, or if you believe they should never happen, you can just let them
 * propagate and turn into fatal errors. </p>
 *
 * @author Patrick Pelletier
 * @author Dan Daly
 *
 * @see DigitalScheduler
 * @see AbstractSyncDevice#nextCycle
 * @see AbstractSyncDevice#send(ChannelOutput, Message)
 * @see AbstractSyncDevice#send(ChannelOutput[], int[])
 * @see AbstractSyncDevice#send(ChannelOutput[], Message[])
 * @see AbstractSyncDevice#receive(ChannelInput)
 * @see AbstractSyncDevice#receive(ChannelInput[])
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public abstract class AbstractSyncDevice extends AbstractDevice {

    public class ChannelFullException extends RuntimeException {
	public ChannelFullException(ChannelOutput out) {
	    super(out.getName());
	}
    }

    public class ChannelEmptyException extends RuntimeException {
	public ChannelEmptyException(ChannelInput in) {
	    super(in.getName());
	}
    }

    private int cycleTime = 1800;

    protected AbstractSyncDevice(String name, boolean suppressOutput) {
	super(name, suppressOutput);
    }

    /** I don't really get this parallel stuff, but I don't think it's
     * applicable to a synchronous device, so don't let anyone call it. */

    private static final String notmean = 
	"() not meaningful for synchronous devices";

    protected void startParallel() {
	throw new UnsupportedOperationException("startParallel" + notmean);
    }

    protected void endParallel() {
	throw new UnsupportedOperationException("endParallel" + notmean);
    }

    protected void parallelSend(ChannelOutput out, Message message)
            throws InterruptedException {
	throw new UnsupportedOperationException("parallelSend" + notmean);
    }

    protected void updateTime(long temptime) {
	throw new UnsupportedOperationException("updateTime" + notmean);
    }

    protected void updateParallelTime(long temptime) {
	throw new UnsupportedOperationException("updateParallelTime" + notmean);
    }

    private void updateTime(long [] times) {
	throw new UnsupportedOperationException("updateTime" + notmean);
    }

    protected void delayBy(long deltaTime) {
	throw new UnsupportedOperationException("delayBy" + notmean);
    }

    /**
     * Increases time by cycleTime, and uses a WakeAt to insure that any events
     * that should have happened by that time have already happened.  You
     * should call this once every time through your main loop, to represent
     * the passing of one cycle. 
     */
    protected void nextCycle() throws InterruptedException {
	super.delayBy(cycleTime);
	new WakeAt(time, new Object()).sleepTil();
    }

    /** Transaction types for sends and receives */
    private TransactionType sendTT = null;
    private TransactionType recvTT = null;

    /** Attribute types for sends and receives */
    private LongAttribute recvAttr = null;
    private LongAttribute sendAttr = null;
    

    // all the sends that takes ints, longs, bools fall thru to this one
    protected void send(ChannelOutput out, Message message)
            throws InterruptedException {
	if (!out.probeSend() || ((BufferedChannel)out).getSendTime() > time)
	    throw new ChannelFullException(out);
    if (opts.loggingTrans()) {
        if (sendTT == null) {
            sendTT = newTransactionType(EVENT, "syncsend");
            sendAttr = new LongAttribute("data", sendTT, opts);
        }
        Transaction tran = beginTransaction(sendTT, "syncsend", out.toString());
        sendAttr.set(new AttributeValue(message.longValue()),getTime());
        message.setSender(tran);
        //Crit Path
        if (lastReceive != null) {
           sigscan.link(lastReceive, tran,getTime());
           lastReceive = null;
           lastReceiveTime = -1;
        }
    }
	out.send(message, time);
    }

    protected void send(ChannelOutput [] out, int [] messages)
            throws InterruptedException {
	for (int i = 0; i < out.length; i++)
	    send(out[i], messages[i]);
    }

    protected void send(ChannelOutput [] out, Message [] messages)
	throws InterruptedException {
	for (int i = 0; i < out.length; i++)
	    send(out[i], messages[i]);
    }

    /**Correctly probes the input**/
    public boolean probeReceive(ChannelInput in) {
        return (in.probeReceive() && 
                ((BufferedChannel)in).getReceiveTime() <= time);
    }

    private Transaction lastReceive = null;
    private long lastReceiveTime = -1;

    protected Message receive(ChannelInput in) throws InterruptedException {
        if (!in.probeReceive() ||
            ((BufferedChannel)in).getReceiveTime() > time)
            throw new ChannelEmptyException(in);
        Message message = in.receive(time);
        if (opts.loggingTrans()) {
            if (recvTT == null) {
                recvTT = newTransactionType(EVENT, "syncreceive");
                recvAttr = new LongAttribute("data", recvTT, opts);
            }
            Transaction tran = 
                beginTransaction(recvTT, "sync receive", in.toString());
            recvAttr.set(new AttributeValue(message.longValue()), getTime());
            link(message, tran);
            if (time > lastReceiveTime) {
                //Updating the last receive transaction for crit path
                lastReceiveTime = time;
                lastReceive = tran;
            }
        }
        return message;
    }

    protected Message [] receive(ChannelInput ins [])
	throws InterruptedException {
	int l = ins.length;
        Message rv [] = new Message [l];
	for (int i = 0; i < l; i++) {
	    rv[i] = receive(ins[i]);
	}
	return rv;
    }

} // end of abstract class AbstractSyncDevice

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
