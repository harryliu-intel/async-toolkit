/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;

import com.avlsi.util.debug.Debug;
import com.avlsi.util.container.Pair;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

/**
 * <p> Class for waiting for incoming messages on channels, node transitions, or
 * timeouts. </p>
 *
 * @see BufferedChannel
 * @see ChannelInput
 * @see ChannelOutput
 *
 * @author Aaron Denney <denney@fulcrummicro.com>
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 *
 * @bug kiniry 23 July 2002 - Memory allocation and garbage
 * collection abuse caused by data-structure misuse.  Any
 * data-structure that is (a) large, (b) static, and (c) is going
 * to be walked frequently needs to provide a non-interator-based
 * interface.
 * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1164">Bug#1164</a>
 **/

public class Wait implements WaiterInterface 
{
    // Private Attributes

    /** listens for nodes going up. **/
    private final NodeWaker upNodeWaker = new NodeWaker(Node.VALUE_1);
    /** listens for nodes going down. **/
    private final NodeWaker downNodeWaker = new NodeWaker(Node.VALUE_0);


    // Protected Attributes

    /** list of current input members of the wait set. **/
    protected final Collection/*<ChannelInput>*/ ins;
    /** list of current output members of the wait set. **/
    protected final Collection/*<ChannelOutput>*/ outs;
    /** list of current nodes waiting to transition up. **/
    protected final Collection/*<Node>*/ ups;
    /** list of current nodes waiting to transition down. **/
    protected final Collection/*<Node>*/ downs;
    /** 
     * Are we currently asleep. 
     *
     * <p> All accesses must be done while monitor for <code>this</code> is
     * held.
     */
    protected boolean waiting = false;


    // Constructors
    
    /**
     * Create a new instance of <code>Wait</code>.
     *
     * One wakeup happens, and removeReady() removes all copies.
     *
     * It is unspecified whether disableRead() and disableWrite()
     * remove all, or merely one copy of duplicates.
     *
     * @param in an array of input channels on which to wait.
     * @param out an array of output channels on which to wait.
     * @param up an array of nodes to wait for going up.
     * @param down an array of nodes to wait for going down.
     *
     * @design The original comment was: Duplicate channels or nodes should be
     * handled "nicely".  This is now a precondition on the constructor.
     *
     * @review kiniry - Given channels can be added, removed, enabled, and
     * disabled over the course of this objects lifecycle, these preconditions
     * are probably slightly too strong.
     *
     * @requires At least one of the parameters must be non-null.
     * @requires For those parameters that are non-null, they must contain at
     * least one element.
     * @requires For those parameters that are non-null, each channel must only
     * be included exactly once.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires (in != null || out != null || up != null || down != null);
     *   requires (in != null ==> (\forall int i; 0 <= i && i < in.length;
     *                                            in[i] != null)) ||
     *            (out != null ==> (\forall int i; 0 <= i && i < out.length;
     *                                             out[i] != null)) ||
     *            (up != null ==> (\forall int i; 0 <= i && i < up.length;
     *                                            up[i] != null)) ||
     *            (down != null ==> (\forall int i; 0 <= i && i < down.length;
     *                                              down[i] != null));
     *   requires (in != null ==> 
     *             (\forall int i,j; 0 <= i && i < in.length &&
     *                               0 <= j && j < in.length && i != j;
     *                               in[i] != in[j]));
     *   requires (out != null ==> 
     *             (\forall int i,j; 0 <= i && i < out.length &&
     *                               0 <= j && j < out.length && i != j;
     *                               out[i] != out[j]));
     *   requires (up != null ==> 
     *             (\forall int i,j; 0 <= i && i < up.length &&
     *                               0 <= j && j < up.length && i != j;
     *                               up[i] != up[j]));
     *   requires (down != null ==> 
     *             (\forall int i,j; 0 <= i && i < down.length &&
     *                               0 <= j && j < down.length && i != j;
     *                               down[i] != down[j]));
     * </jml></pre>
     **/

    public Wait(ChannelInput [] in, ChannelOutput [] out, Node [] up, Node [] down) {
        ins = new HashSet/*<ChannelInput>*/();
        if (in != null) {
            for (int i = 0; i < in.length; i++){
                ins.add(in[i]);
            }
        }

        outs = new HashSet/*<ChannelOutput>*/();
        if (out != null) {
            for (int i = 0; i < out.length; i++){
                outs.add(out[i]);
            }
        }

        ups = new HashSet/*<Node>*/();
        if (up != null) {
            for (int i = 0; i < up.length; i++){
                ups.add(up[i]);
            }
        }

        downs = new HashSet/*<Node>*/();
        if (down != null) {
            for (int i = 0; i < down.length; i++){
                downs.add(down[i]);
            }
        }


    }

    /**
     * Create a new instance of <code>Wait</code>.
     *
     * @param in an array of input channels on which to wait.
     * @param out an array of output channels on which to wait.
     *
     * @review kiniry - Given channels can be added, removed, enabled, and
     * disabled over the course of this objects lifecycle, these preconditions
     * are probably slightly too strong.
     *
     * @requires At least one of the parameters must be non-null.
     * @requires For those parameters that are non-null, they must contain at
     * least one element.
     * @requires For those parameters that are non-null, each channel must only
     * be included exactly once.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires (in != null || out != null);
     *   requires (in != null ==> (\forall int i; 0 <= i && i < in.length;
     *                                            in[i] != null)) ||
     *            (out != null ==> (\forall int i; 0 <= i && i < out.length;
     *                                             out[i] != null));
     *   requires (in != null ==> 
     *             (\forall int i,j; 0 <= i && i < in.length &&
     *                               0 <= j && j < in.length && i != j;
     *                               in[i] != in[j]));
     *   requires (out != null ==> 
     *             (\forall int i,j; 0 <= i && i < out.length &&
     *                               0 <= j && j < out.length && i != j;
     *                               out[i] != out[j]));
     * </jml></pre>
     **/

    public Wait(ChannelInput [] in, ChannelOutput [] out) {
        this(in, out, null, null);
    }

    /**
     * Waits until an object in the wait set is "ready".  Equivalent
     * to <code>(Waitable) select2().getFirst()</code>.
     *
     * @see BufferedChannel
     * @see ChannelInput
     * @see ChannelOutput
     * 
     * @return one of the ready resources.
     * @exception EmptyWaitSetException When?
     * @exception InterruptedException When?
     *
     * <p> Not terribly useful, and should be replaced with something
     * more useful. unfortunately we can't tell whether a channel is
     * returned for reading or writing.
     *
     * deprecated  Use {@link #select2} instead.  There is no way of
     *     knowing the time the event happened with this method.
     **/
    public /*@ non_null @*/ Waitable select() throws InterruptedException {
        return (Waitable) select2().getFirst();
    }

    /**
     * Waits until an object in the wait set is "ready".
     *
     * @see BufferedChannel
     * @see ChannelInput
     * @see ChannelOutput
     * 
     * @return one of the ready resources.
     * @exception EmptyWaitSetException When?
     * @exception InterruptedException When?
     *
     * <p> Not terribly useful, and should be replaced with something
     * more useful. unfortunately we can't tell whether a channel is
     * returned for reading or writing.
     *
     * <p> Sketch of locking correctness:
     *
     * <p> To avoid deadlock, when we have our lock, we can't call anything
     * requiring a lock while something holding that lock might want to
     * acquire our lock.
     *
     * <p> anyReady() should not require locking.
     * <p> setWaiters() and clearWaiters() need to lock to protect the
     * waiting status, and this will interact with read/write calls.
     * However, while we are calling setWaiters(), it is not possible
     * for the waitees to be trying to grab our lock, as they don't
     * have a reference to us.  So, only clearWaiters() need be outside
     * the synchronized block.
     *
     * <p>the things that can wake us basically do:
     *
     * <pre>
     * if (possible to be blocked) {
     *     if (in select) {
     *         Wait.wakeUp();
     *     }
     * }
     * </pre>
     *
     * <p> It will be possible that anyReady() becomes true while we are
     * setting the waiters. If so, then we shouldn't sleep, but we also
     * shouldn't let anyone try to wake us. The same goes for adjusting
     * our thread count up or down. So, we protect the sleeping and the
     * thread counting by checking against anyReady(). Since we don't
     * sleep no one can try to wake us or adjust the thread count until
     * after the synchronized block exits, at which point waiting is now
     * false.
     **/
    public /*@ non_null @*/ Pair/*<Waitable,Long>*/ select2()
        throws InterruptedException {

        // Cached time to compute how much time we spend waiting.
        long timeOfSelectCall;

        // Get the time that this select() was called.
        if (Statistics.STATISTICS)
            timeOfSelectCall = System.currentTimeMillis();
        // We must increment the total select() time when this method returns.

        // Avoid locking, if necessary.
        if (!nonEmpty())
            throw new EmptyWaitSetException();
        Pair/*<Waitable,Long>*/ p = anyReady();
        if (p != null) {
            if (Statistics.STATISTICS)
                Statistics.incrementSelectWaitTime(System.currentTimeMillis() -
                                                   timeOfSelectCall);
            return p;
        }
        try {
            // commited to expensive path now
            synchronized (this) {
                waiting = true;
                setWaiters();
                p = anyReady();
                if (p == null) {
                    DigitalScheduler.get().decThreadCount();
                    while (waiting) {
                        wait();
                    };
                    p = anyReady();
                }
                // Asserting no false wake-ups...
                Debug.assertTrue(p != null);
                waiting = false;
            }
        }
        finally {
            clearWaiters();
        }

        // Gather statistics on this select call.
        if (Statistics.STATISTICS)
            Statistics.incrementSelectWaitTime(System.currentTimeMillis() -
                                               timeOfSelectCall);

        return p;
    }

    /**
     * Used by channels to wake up a selector.
     *
     * <p> Needed to ensure multiple channels don't each increment the
     * count of awake threads.
     * 
     * @concurrency GUARDED Why?
     */
    public synchronized void wakeUp() {
        if (waiting) {
            DigitalScheduler.get().incThreadCount();
            waiting = false;
            notify();
        }
    }

    /**
     * Are any ready?.
     *
     * @bug What does this comment (above) mean?
     *
     * @bug kiniry 23 July 2002 - Memory allocation and garbage
     * collection abuse caused by data-structure misuse.  Any
     * data-structure that is (a) large, (b) static, and (c) is going
     * to be walked frequently needs to provide a non-interator-based
     * interface.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1164">Bug#1164</a>
     **/
    private Pair/*<Waitable,Long>*/ anyReady() {
        for (Iterator i = ins.iterator(); i.hasNext();) {
            ChannelInput in = (ChannelInput)i.next();
            if (in.probeReceive()) {
                return new Pair/*<Waitable,Long>*/(in,
                        new Long(in.getReceiveTime()));
            }
        }

        for (Iterator i = outs.iterator(); i.hasNext();) {
            ChannelOutput out = (ChannelOutput)i.next();
            if (out.probeSend()) {
                return new Pair/*<Waitable,Long>*/(out,
                        new Long(out.getSendTime()));
            }
        }

        for (Iterator i = ups.iterator(); i.hasNext();) {
            Node up = (Node)i.next();
            if (up.getValue() == Node.VALUE_1 ) {
                return new Pair/*<Waitable,Long>*/(up,
                        new Long(up.getTime()));
            }
        }

        for (Iterator i = downs.iterator(); i.hasNext();) {
            Node down = (Node)i.next();
            if (down.getValue() == Node.VALUE_0) {
                return new Pair/*<Waitable,Long>*/(down,
                        new Long(down.getTime()));
            }
        }

        return null;
    }

    /**
     * @todo Undocumented.
     *
     * @bug kiniry 23 July 2002 - Memory allocation and garbage
     * collection abuse caused by data-structure misuse.  Any
     * data-structure that is (a) large, (b) static, and (c) is going
     * to be walked frequently needs to provide a non-interator-based
     * interface.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1164">Bug#1164</a>
     **/
    protected void setWaiters() {
        for (Iterator i = ins.iterator(); i.hasNext();) {
            ((ChannelInput)i.next()).setReadWaiter(this);
        }

        for (Iterator i = outs.iterator(); i.hasNext();) {
            ((ChannelOutput)i.next()).setWriteWaiter(this);
        }

        for (Iterator i = ups.iterator(); i.hasNext();) {
            ((Node)i.next()).addWatch(upNodeWaker);
        }

        for (Iterator i = downs.iterator(); i.hasNext();) {
            ((Node)i.next()).addWatch(downNodeWaker);
        }
    }

    /**
     * @todo Undocumented.
     *
     * @bug kiniry 23 July 2002 - Memory allocation and garbage
     * collection abuse caused by data-structure misuse.  Any
     * data-structure that is (a) large, (b) static, and (c) is going
     * to be walked frequently needs to provide a non-interator-based
     * interface.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1164">Bug#1164</a>
     **/
    protected void clearWaiters() {
        for (Iterator i = ins.iterator(); i.hasNext();) {
            ((ChannelInput)i.next()).clearReadWaiter();
        }

        for (Iterator i = outs.iterator(); i.hasNext();) {
            ((ChannelOutput)i.next()).clearWriteWaiter();
        }

        for (Iterator i = ups.iterator(); i.hasNext();) {
            ((Node)i.next()).removeWatch(upNodeWaker);
        }

        for (Iterator i = downs.iterator(); i.hasNext();) {
            ((Node)i.next()).removeWatch(downNodeWaker);
        }
    }

    /**
     * Adds the input channels specified to the list of
     * channels to wait for, or enables disabled channels.
     **/
    public void addRead(ChannelInput [] newins) {
        for (int i = 0; i < newins.length; i++) {
            ins.add(newins[i]);
        }
    }

    /**
     * Adds the input channel specified to the list of
     * channels to wait for, or enable a disabled channel.
     **/
    public void addRead(ChannelInput in) {
        ins.add(in);
    }

    /**
     * Adds the input channels specified to the list of
     * channels to wait for, or enables disabled channels.
     **/
    public void addWrite(ChannelOutput [] newouts) {
        for (int i = 0; i < newouts.length; i++) {
            outs.add(newouts[i]);
        }
    }

    /**
     * Adds the input channel specified to the list of
     * channels to wait for, or enable a disabled channel.
     **/
    public void addWrite(ChannelOutput out) {
        outs.add(out);
    }

    /**
     * Adds the node specified to the list of nodes to wait for.
     **/
    public void addUp(Node up) {
        ups.add(up);
    }

    /**
     * Adds the node specified to the list of nodes to wait for.
     **/
    public void addDown(Node down) {
        downs.add(down);
    }

    /**
     * Remove the given channel from the wait set.
     **/
    public void disableRead(ChannelInput in) {
        ins.remove(in);
    }

    /**
     * Remove the given channel from the wait set.
     **/
    public void disableWrite(ChannelOutput out) {
        outs.remove(out);
    }

    /**
     * Remove the given node from the wait set.
     **/
    public void disableUp(Node up) {
        ups.remove(up);
    }

    /**
     * Remove the given node from the wait set.
     **/
    public void disableDown(Node down) {
        downs.remove(down);
    }

    /**
     * @todo Undocumented.
     *
     * @bug kiniry 23 July 2002 - Memory allocation and garbage
     * collection abuse caused by data-structure misuse.  Any
     * data-structure that is (a) large, (b) static, and (c) is going
     * to be walked frequently needs to provide a non-interator-based
     * interface.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1164">Bug#1164</a>
     */
    public void removeReady() {
        for (Iterator i = ins.iterator(); i.hasNext();) {
            if (((ChannelInput)i.next()).probeReceive()) {
                i.remove();
            }
        }

        for (Iterator i = outs.iterator(); i.hasNext();) {
            if (((ChannelOutput)i.next()).probeSend()) {
                i.remove();
            }
        }

        for (Iterator i = ups.iterator(); i.hasNext();) {
            if (((Node)i.next()).getValue() == Node.VALUE_1) {
                i.remove();
            }
        }

        for (Iterator i = downs.iterator(); i.hasNext();) {
            if (((Node)i.next()).getValue() == Node.VALUE_0) {
                i.remove();
            }
        }
    }

    /**
     * @todo Undocumented.
     */
    protected /*@ pure @*/ boolean nonEmpty() {
        return !(ins.isEmpty() && outs.isEmpty() &&
                 ups.isEmpty() && downs.isEmpty());
    }


    // Inner Classes

    /**
     * @todo Undocumented.
     **/

    private class NodeWaker implements NodeWatcher {
        private final int value;
        public NodeWaker(int value) {
            this.value = value;
        }

        public void nodeChanged(Node n, long time) {
            if (n.getValue() == value) {
                if ((ups.contains(n) && value == 1)
                    || (downs.contains(n) && value == 0)) {
                    wakeUp();
                }
            }
        }
    }

} // end of class Wait

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
