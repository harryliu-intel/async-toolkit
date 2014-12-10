/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.DebugOpts;

import com.avlsi.tools.sigscan.Transaction;
import com.avlsi.tools.sigscan.TransactionType;
import com.avlsi.tools.sigscan.LongAttribute;
import com.avlsi.tools.sigscan.AttributeValue;

/**
 * Class for converting messages on channels inito bus
 * signals, and vice versa
 *
 * This class is NOT a thread, so don't expect to put in a go()
 * method and start it with .start().
 *
 * This class is similar to AbstractSyncDevice, but with a few 
 * differences:<br>
 * <ul>
 * <li>SynchronousConverter in not threaded.
 * <li>SynchronousConverter does not have its own local time, its time is
 * synced with the scheduler.
 * <li>The biggest difference is that SynchronousConverters are meant to wait
 * on an explicit Clock, instead of calling nextCycle (which runs 
 * AbstractSyncDevice to its own implicit clock cycle)
 * </ul>
 *
 * @author Dan Daly
 * @version $Date$
 **/

public abstract class SynchronousConverter extends StateMachine {

    /**
     * Constructor that sets up the StateMachine.
     *
     * @param scopename the scope to place this object in.
     * @param name the name of this object.
     * @param sigscan the database to place this in (can be null).
     * @param opts the database options.
     **/
    public SynchronousConverter(String scopename, String name,
                                Sigscan sigscan, DebugOpts opts) {
        super(scopename, name, sigscan, opts);
    }

    /********************************************************************/

    /** Send Methods **/
    
    protected boolean probeSend(ChannelOutput out) {
        return (out.checklessProbeSend() &&
                (out.getSendTime() <= getTime()));
    }
    
    protected void send(ChannelOutput out, Message message) {
        
        if (!probeSend(out)) throw new ChannelFullException(out);
        
        if (opts.loggingTrans()) logSend(out, message);
        
        try { 
            ((BufferedChannel)out).checklessSend(message, getTime());
        } catch (InterruptedException e) {
            handleInterruptedException(e);
        }
    }

    /********************************************************************/

    /** Receive Methods **/
    
    protected boolean probeReceive(ChannelInput in) {
        return (in.checklessProbeReceive() &&
                (in.getReceiveTime() <= getTime()));
    }

    protected Message receive(ChannelInput in) {
        
        if (!probeReceive(in)) throw new ChannelEmptyException(in);
        
        Message message = null;
        try {
            message = ((BufferedChannel)in).checklessReceive(getTime());
        } catch (InterruptedException e) {
            handleInterruptedException(e);
        }
        
        if (opts.loggingTrans()) logReceive(in, message);
        
        return message;
    }
    
    /********************************************************************/

    /** Channel RuntimeExceptions **/
    
    public class ChannelFullException  // see bug 1737
        extends com.avlsi.tools.tsim.ChannelFullException {
        public ChannelFullException(ChannelOutput out) {
            super(out);
        }
    }

    public class ChannelEmptyException extends RuntimeException {
        public ChannelEmptyException(ChannelInput in) {
            super(in.getName());
        }
    }

    /********************************************************************/

    /** Exception Handling **/
    
    protected void handleInterruptedException(InterruptedException e) {
        System.err.println("Interrupted Exception: "+e.getMessage());
        e.printStackTrace();
        System.exit(1);
    }

    /********************************************************************/

    /** Variables Used for Logging **/
    
    /** Transaction types for sends and receives */
    private TransactionType sendTT = null;
    private TransactionType recvTT = null;

    /** Attribute types for sends and receives */
    private LongAttribute recvAttr = null;
    private LongAttribute sendAttr = null;

    private Transaction lastReceive = null;
    private long lastReceiveTime = -1;

    /** Logging Functions **/

    protected void logSend(ChannelOutput out, Message message) {
        if (sendTT == null) {
            sendTT = newTransactionType(EVENT, "syncsend");
            sendAttr = new LongAttribute("data", sendTT, opts);
        }
        Transaction tran = 
            beginTransaction(sendTT, "syncsend", out.toString());
        sendAttr.set(new AttributeValue(message.longValue()),getTime());
        message.setSender(tran);
        //Crit Path
        if (lastReceive != null) {
           link(lastReceive, tran);
           lastReceive = null;
           lastReceiveTime = -1;
        }
    }

    protected void logReceive(ChannelInput in, Message message) {
        if (recvTT == null) {
            recvTT = newTransactionType(EVENT, "syncreceive");
            recvAttr = new LongAttribute("data", recvTT, opts);
        }
        Transaction tran = 
            beginTransaction(recvTT, "sync receive", in.toString());
        recvAttr.set(new AttributeValue(message.longValue()), getTime());
        link(message.getSender(), tran);
        if (getTime() > lastReceiveTime) {
            //Updating the last receive transaction for crit path
            lastReceiveTime = getTime();
            lastReceive = tran;
        }
    }

    protected void link(Transaction tran1, Transaction tran2) {
        if ((tran1 !=null) && (tran2 != null)) {
            sigscan.link(tran1, tran2, getTime());
        }
    }
}

