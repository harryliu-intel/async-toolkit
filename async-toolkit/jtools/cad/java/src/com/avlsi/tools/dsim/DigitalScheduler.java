/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import EDU.oswego.cs.dl.util.concurrent.BrokenBarrierException;
import EDU.oswego.cs.dl.util.concurrent.CyclicBarrier;

import com.avlsi.tools.dsim.EventQueue;
import com.avlsi.tools.dsim.RandomEventQueue;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.AbstractDeviceThread;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

import java.util.ConcurrentModificationException;

//import com.avlsi.util.container.PriorityQueue;
//import com.avlsi.util.container.RandomQueue;

/**
 * This is the entity that controls an associated TSim simulator, waiting for it
 * to reach stability before taking another action in DSim.
 *
 * @author tim
 * @author Aaron Denney
 * @author jlm
 * @author Dan Daly
 * @author Jesse Rosenstock
 * @author Chris Brichford
 * @author Kim Wallmar
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 *
 * @review kiniry 18-23 July 2002
 **/

public class DigitalScheduler {

    /* One instance of class. */
    private static DigitalScheduler singleton = null;
    /**
     * Thread synchronization lock object.  The monitor for
     * <code>this</code> not be acquired while holding <code>tLock</code>.
     * This lock must be held while accessing <code>tCount</code>.
     * This is not obeyed in <code>clear</code>.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=3635">
     * Bug#3635</a>
     **/
    private Object tLock = new Object();
    /** 
     * Active simulation thread count. Digital simulator waits on all
     * other simulation threads.
     *
     * <p>The lock <code>tLock</code> must be held while accessing
     * <code>tCount</code>. 
     *
     * <p> The DigitalScheduler will not fire any events until
     * <code>tCount</code> has reached 0.  Assuming other classes
     * manipulate the thread count at appropriate times, this ensures
     * that all devices will eventually block either on a channel
     * communication or waiting for a node to transistion without
     * performing any channel or node actions that could unblock
     * another device.  Similarly, the thread count must be incremented
     * before the device is awakened.
     *
     * <p> The DigitalScheduler must wait for stability because if it
     * did not, the devices running in parallel with it could schedule
     * events in the past, which would cause performance inaccuracies.
     * The DigitalScheduler's time can never go back, so the events
     * would happen later than they were intended to.  
     *
     * <p> While calling <code>decThreadCount()</code>, the lock for
     * the object on which the thread will wait must be held in order
     * to prevent the thread from being notified before it has waited.
     * Following this rule will prevent dsim from falsely detecting
     * deadlock, as the prototype tsim3 implementation could do.
     * The lock need not be held while calling <code>incThreadCount</code>
     * because it is always safe to have the thread count larger than it
     * "really" is.
     */
    private int tCount = 0;

    /**
     * List of barriers used by threads waiting for the event queue to be
     * empty.
     **/
    private final LinkedList/*<CyclicBarrier>*/ emptyEventQueueBarriers =
        new LinkedList/*CyclicBarrier*/();

    /** Priority queue for timed events. Synchronized on this. **/
    private EventQueueInterface pqueue = //new EventQueue();
        new BinningQueue();

    /** Random queue for untimed events. Synchronized on this. **/
    private EventQueueInterface rqueue;

    private final Random rand;
    /** The time that the last event fired. **/
    private long lastTime = 0;

    /** Whether to stall the simulation before the next event. **/
    private InterruptedBy interrupted = InterruptedBy.NONE;

    /** 
     * Dynamic debugging flag for extreme logging verbosity. 
     **/
    private boolean verbose = false;

    private boolean canAddDevices = true;

    /**
     * All of the devices that need to be dealt with on clear.  Maps from
     * instance name to device. All access should be guarded by
     * synchronizing on <code>this</code>.
     *
     * <pre><jml> 
     * private invariant deviceMap != null; 
     * </jml></pre>
     **/

    private final /*@ non_null @*/ Map<String,AbstractDevice> deviceMap;

    /**
     * Possible sources of interruption.  Fill in additional values as needed.
     **/
    public static enum InterruptedBy {
        NONE,
        USER,
        OTHER
    }

    /**
     * The thread that constructed this object.  Used to give asserts about
     * threads that add event, but don't extend AbstractDeviceThread.
     **/
    private final Thread constructorThread;

    /**
     * Threads that have added to the event queue in this cycle.
     **/
    private final List<AbstractDeviceThread> activeThreads =
        new ArrayList<AbstractDeviceThread>();

    // Constructors

    /**
     * Create a new DigitalScheduler.
     **/

    private DigitalScheduler(long seed) {
        rand = new Random(seed);
        rqueue = new RandomEventQueue(rand);
        deviceMap = new HashMap<String,AbstractDevice>();
        this.constructorThread = Thread.currentThread();
    }

    // Methods

    /** 
     * @return a singleton instance of scheduler. 
     **/

    public static synchronized DigitalScheduler get() { 
        if (singleton == null)
            singleton = new DigitalScheduler(1);
        return singleton; 
    }

    /** Controls printing of extra info **/
    public void setVerbose(boolean v) { verbose = v; }

    public final synchronized void addDevice(AbstractDevice newDevice,
                                             String name) {
        Debug.assertTrue(!deviceMap.containsKey(name));
        Debug.assertTrue(!deviceMap.containsValue(newDevice));

        if (canAddDevices) {
            deviceMap.put(name, newDevice);
        } else {
            throw new ConcurrentModificationException("Don't call addDevice " +
                                                      "while doing a DSim.clear");
        }

        incThreadCount();
    }

    public void incThreadCount(int count) {
        synchronized(tLock) {
            if (verbose)
                System.err.println("inc: " + tCount + "->" + (tCount + count));
            tCount += count;
        }
    }

    /** 
     * Another simulation thread is active. 
     *
     * @design This method is not synchronized because
     * {@link #incThreadCount(int)} handles it.
     **/

    public void incThreadCount() {
        incThreadCount(1);
    }

    /** 
     * Another simulation thread is finished. 
     *
     * <p> Acquires <code>tLock</code>.
     **/
    public void decThreadCount() {
        // Thread.dumpStack();
        synchronized (tLock) {
            if (verbose)
                System.err.println("dec: " + tCount + "->" + (tCount - 1));
            tCount--;
            if (tCount==0) { tLock.notifyAll(); }
            // deal with under counting
            else if (tCount<0) {
                throw new 
                    IllegalStateException("Digital Simulator Thread Count < 0 : " + 
                                          tCount);
            }
        }
    }
    /** Wait for all other simulation threads to stop, before cycling. **/
    private void waitForAllThreads() {
        synchronized (tLock) {
            while (tCount!=0) { 
                try { tLock.wait(); }
                catch (InterruptedException ie) {
                    Debug.fail();
                }
            }
        }
        addDeviceEvents();
    }
    /** Checks for an active thread **/
    private boolean threadsActive() {
        if (verbose) System.err.println("Checking for thread activity.");
        synchronized (tLock) {
            if (verbose) System.err.println("Lock aquired.");
            return tCount > 0;
        }
    }

    /** 
     * Adds an event to the appropriate queue.
     *
     * @param e the event to add.
     **/
    public void addEvent(Event e) {
        final Thread currThread = Thread.currentThread();
        if (currThread instanceof AbstractDeviceThread) {
            final AbstractDeviceThread th = (AbstractDeviceThread) currThread;
            if (!th.getProcessed()) {
                th.setProcessed(true);
                synchronized (activeThreads) {
                    activeThreads.add(th);
                }
            }
            th.addEvent(e);
        } else {
            insertEvent(e);
        }
    }

    /**
     * Add all pending events from per thread event queues to the central event
     * queue.
     **/
    private void addDeviceEvents() {
        // the abstract device threads synchronizes on tLock when decrementing
        // the thread count; this thread synchronizes on tLock while waiting
        // for tCount to become 0; thus all modifications to activeThreads must
        // be visible in this thread without additional synchronization
        final int size = activeThreads.size();
        if (size > 0) {
            if (size > 1) {
                // except at reset, most times activeCount is 1
                Collections.sort(activeThreads);
            }
            for (int i = 0; i < size; ++i) {
                final AbstractDeviceThread th = activeThreads.get(i);
                assert th.getProcessed();
                th.setProcessed(false);

                final List<Event> events = th.getEvents();
                final int length = events.size();
                for (int j = 0; j < length; ++j) {
                    insertEvent(events.get(j));
                }
                th.clearEvents();
            }
            activeThreads.clear();
        }
    }

    /** 
     * Adds an event to the appropriate queue.  
     *
     * @param e the event to add.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires e != null;
     *   requires (e instanceof SequencedEvent) ==>
     *            ((SequencedEvent) e).hasNextEvent() == false;
     * </jml></pre>
     *
     * @bug FIXME: check if on either queue?  Or keep that responsibility in
     * client?
     **/
    private void insertEvent(Event e) {
        // not synchronzied because this method should only be called by 1
        // thread
        assert Thread.currentThread() == constructorThread;

        if (verbose)
            System.err.println(e.getTime() + ": Adding " + e + 
                               " index = " + e.getIndex());
        if ((e instanceof SequencedEvent)  && ((SequencedEvent) e).hasNextEvent())
            System.err.println("Event has NEXT!!");

        boolean isQueued = e.getIndex() >= 0;

        if (verbose) 
            if (e.getTime() < getTime()) 
                System.err.println("Queueing Up Event "+e+" which is in the past "+
                                   e.getTime()+"<"+getTime());
        /*if (e.isRandom()) {
         if (!isQueued) { rqueue.enqueue(e); }
         } else {
         if (!isQueued) { pqueue.enqueue(e); }
         else { pqueue.rerank(e); } 
         }*/
        if (e.isRandom()) {
            if (!isQueued) //rqueue.remove(e);
                rqueue.enqueue(e);
        } else {
            if (isQueued) pqueue.remove(e);
            pqueue.enqueue(e);
        }
            
    }

    /** Removes an event from the appropriate queue  */
    public synchronized void removeEvent(Event e) {
        if (e.getIndex()>=0) {
            if (e.isRandom()) { rqueue.remove(e); }
            else { pqueue.remove(e); }
        }
    }
    /** Runs up to, but not including events at <code>time</code>. */
    public void cycleTo(long time) {
        interrupted = InterruptedBy.NONE;
        // Cannot choose next event until threads are inactive.
        waitForAllThreads();
        while (interrupted == InterruptedBy.NONE && hasMoreEvents() &&
               isPqueueInFuture(time)) {
            doOneCycle();
            // Cannot choose next event until threads are inactive.
            waitForAllThreads();
        }
    }

    //public int getNextEventTime() {
    public long getNextEventTime() {
        // Cannot look at next event until threads are inactive.
        waitForAllThreads();
        long ret = 0;
        synchronized(this) {
            if (pqueue.isEmpty()) {
                if (verbose) 
                    System.out.println("pqueue is empty @"+getTime());
                return -1;
            }
            ret = pqueue.front().getTime();
        }
        return ret;
    }

    /** Waits until no threads are running in Tsim **/
    public synchronized void waitUntilStable() {
        waitForAllThreads();
    }

    /**
     * Misnomer.  But we want to keep firing random queue
     * if timed queue is empty.
     **/
    //private boolean isPqueueInFuture(int time) {
    private boolean isPqueueInFuture(long time) {
        return pqueue.isEmpty() || compareTime(pqueue.front().getTime(),time) < 0;
    }

    /** Runs until there are no more events in the queue (and no more threads). */
    public void cycle() {
        interrupted = InterruptedBy.NONE;
        waitForAllThreads();
        while (interrupted == InterruptedBy.NONE && hasMoreEvents()) {
            doOneCycle(); 
            waitForAllThreads();
        }
    }

    /** 
     * Runs until 'node' has the 'tCount' transitions or there are no more
     * events in the queue.
     *
     * @param node the node to check.
     * @param tCount the number of transitions to cycle until.
     **/

    public void cycle(Node node, int tCount) {
        if (verbose) {
            System.err.println("Cycling node for " + tCount +" counts");
        }
        tCount += node.getTCount();
        interrupted = InterruptedBy.NONE;
        waitForAllThreads();
        while (interrupted == InterruptedBy.NONE && hasMoreEvents() &&
               node.getTCount() < tCount) {
            doOneCycle();
            waitForAllThreads();
        }
    }
    /** Processes the first event in the queues. */
    public void cycleOnce() {
        interrupted = InterruptedBy.NONE;
        doOneCycle();
        waitForAllThreads();
    }
    /**
     * Waits for any exteral simulation threads to complete and processes the
     * next event. 
     **/
    private void doOneCycle() {
        if (DSimDebug.DEBUG)
            Debug.assertTrue(threadCount() == 0);
        // check for interrupts
        if (interrupted != InterruptedBy.NONE) { return; }

        // get our next victim...err event
        Event e = getNextEvent();
        if (e==null) {
            final CyclicBarrier barrier;
            synchronized (this) {
                barrier = emptyEventQueueBarriers.isEmpty() ?
                    (CyclicBarrier) null :
                    (CyclicBarrier) emptyEventQueueBarriers.removeFirst();
            }
            if (barrier != null) {
                try {
                    // release the waiting thread from the barrier
                    barrier.barrier();
                    // and wait for him to increment his thread count
                    barrier.barrier();
                } catch (InterruptedException x) {
                    throw new AssertionError(x);
                } catch (BrokenBarrierException x) {
                    barrier.restart();
                }
            }

            return;
        }
        // process event
        long newTime = e.getTime();
        // There is no monotonicity requirement on event times.
        // @see <a
        // href="http://internal/bugzilla/show_bug.cgi?id=1062">Bug#1062</a>
        // for more information.
//         if (verbose) {
//             System.err.println("Current timing information:\n" +
//                                "\tmean = " + AbstractDevice.meanTimeOfAllDevices() +
//                                "\tvariance = " + 
//                                AbstractDevice.varianceOfTimeOfAllDevices());
//         }
        // FIXME this may not handle time with random events correctly...
        if (!e.isRandom() && compareTime(newTime, lastTime) > 0) {
            if (verbose) {
                System.err.println(lastTime + ": changing time to " + newTime);
            }
            lastTime = newTime;
        }
        e.fire();
        if (verbose) System.err.println(e.getTime() + " Fired " + e + 
                                        " numTimed= " + pqueue.size());
    }
    /** Returns the number of queued random events. **/
    public synchronized int randomCount() { return rqueue.size(); }
    /** Returns the number of queued timed events. **/
    public synchronized int timedCount() { return pqueue.size(); }
    /** Returns string of all pending events. Non-node events are ignored. **/
    public synchronized String pendingList() {
        return pqueue.pendingList()+"\n"+rqueue.pendingList();
    }

    /** Returns true if there are any pending events in either queue */
    public synchronized boolean hasMoreEvents() {
        return pqueue.size() > 0 ||
               rqueue.size() > 0 ||
               !emptyEventQueueBarriers.isEmpty();
    }

    /** 
     * @return the next event, but does not remove it. 
     **/
    private synchronized Event getNextEvent() {
        int pSz = pqueue.size(), rSz = rqueue.size(), tot = pSz+rSz;

        if (DSimDebug.DEBUG)
            Debug.log("Pending events when getting next event:\n" + 
                      pqueue.pendingList());

        if (tot>0) {
            /*
             * Handle after 0 rules specially.  They are enqueud with 
             * delay of -1, so they happen "in the past".  They must
             * happen in preference to random rules to avoid instabilities.
             */
            if (pSz > 0 &&
                compareTime(pqueue.front().getTime(),lastTime) < 0) {
                return pqueue.next();
            }
            float pick = rand.nextInt(tot);
            if (pick<rSz) { return rqueue.next(); }
            else { return pqueue.next(); }
        }
        return null;
    }

    /** Stop the current simulation (as soon as possible). */
    public void interrupt() { interrupt(InterruptedBy.OTHER); }
    public void interrupt(InterruptedBy what) { interrupted = what; }

    /** Whether the simulation was interrupted. **/
    public InterruptedBy isInterrupted() { return interrupted; }

    /** Returns time of last event processed. */
    public long getTime() { return lastTime; }

    public void waitForOthersIdle() {
        synchronized (tLock) {
            if (tCount == 1) { return; }
            decThreadCount();
            while (tCount!=0) { 
                try { tLock.wait(); }
                catch (InterruptedException ie) {
                    Debug.fail();
                }
            }
            incThreadCount();
        }
    }

    /**
     * Waits for the event queue to be empty and all threads to be inactive.
     * Only one thread may be waiting for the event queue to be empty
     * at a time.
     *
     * @throws IllegalStateException If there is already a thread waiting
     *   for the event queue to be empty.
     **/
    public void waitForEmptyEventQueue() throws InterruptedException {
        final CyclicBarrier barrier = new CyclicBarrier(2);
        synchronized (this) {
            emptyEventQueueBarriers.add(barrier);
        }

        decThreadCount();

        try {
            // wait for the queue to become empty
            barrier.barrier();
        } finally {
            incThreadCount();
        }

        // signal that we have incremented the thread count
        barrier.barrier();
    }

    public int threadCount() {
        synchronized (tLock) {
            return tCount;
        }
    }

    /**
     * Convert the ordering between first and second to an integer with
     * the same conventions as {@link java.util.Comparator#compare}, etc.
     * Handles time wrapping properly.
     **/
    //public static final int compareTime(final int first, final int second) {
    public static final long compareTime(final long first, final long second) {
        return (first - second);
    }

    /*
     @deprecated Need to fix module reset scheme, such that this method would kill devices
     and then notify modules that their devices were killed.
     */
    public final synchronized void clearEventQueues() {
        clearEventQueuesAtTime(0);
    }

    /*
     * Sets the internal simulator time to time, clears event queues
     * to avoid possible weird time interactions.
     *
     * @deprecated Need to fix module reset scheme, such that this
     * method would kill devices and then notify modules that their
     * devices were killed.
     **/
    public final synchronized void clearEventQueuesAtTime(long time) {
       
        while (!pqueue.isEmpty()) {
            pqueue.next();
        }
        while (!rqueue.isEmpty()) {
            rqueue.next();
        }
        lastTime = time;
    }

    /**
     * Waits forever, will never return normally, only by throwing
     * InterruptedException.  Should never be called by the scheduler,
     * only by threads that wish to sleep forever.
     **/
    public void waitForever() throws InterruptedException {
        // Decrement the thread count, then wait forever on an object
        // that will never be notified.
        final Object o = new Object();
        decThreadCount();
        synchronized (o) {
            o.wait();
        }
        throw new AssertionFailure("Unexpected return from wait!");
    }

    /**
     * Remove all events from the event queues.
     *
     * This should only be used during reset situations
     * (or for debugging this framework)
     **/
    public final void clear() {
        final Map<String,AbstractDevice> deviceMapCopy;
        int myCount;
        
        synchronized (this){
            while (!pqueue.isEmpty()) {
                pqueue.next();
            }
            while (!rqueue.isEmpty()) {
                rqueue.next();
            }

            lastTime = 0;
        
            deviceMapCopy =
                new HashMap<String,AbstractDevice>(deviceMap);
            // tLock should probably be held here to read tCount.
            // @see <a href="http://internal/bugzilla/show_bug.cgi?id=3635">
            // Bug#3635</a>
            myCount = tCount;
            canAddDevices = false;
        }
        boolean shouldExit = false;
        for (AbstractDevice currDevice : deviceMapCopy.values()) {
            currDevice.interrupt();

            try {
                currDevice.join(2000);
                if (currDevice.isAlive()) {
                    Debug.assertTrue(myCount != 0);
                    --myCount;
                    System.err.println("Unable to kill a " + currDevice.getName() + 
                                       " device.");
                    System.err.println("The code for " + currDevice.getName() + 
                                       " executed for more than two seconds " + 
                                       "without waiting on an object.");
                    shouldExit = true;
                }

            }
            catch(InterruptedException e) {
                throw (AssertionFailure)
                    new AssertionFailure("Someone interrupted DSIM.")
                        .initCause(e);
            }
        }
        if (shouldExit) {
            System.err.println("You really should exit DSim as soon as possible.");
        }
        synchronized (this){
            deviceMap.clear();
            canAddDevices = true;
            for (Iterator i = emptyEventQueueBarriers.iterator();
                 i.hasNext(); ) {
                ((CyclicBarrier) i.next()).restart();
            }
            emptyEventQueueBarriers.clear();
        }
        // This is a hack.  It might get around bug 831.
        // In normal cases, the thread count should be 0.
        synchronized (tLock) {
            tCount = 0;
        }

        synchronized (activeThreads) {
            for (int i = 0; i < activeThreads.size(); ++i) {
                final AbstractDeviceThread th = activeThreads.get(i);
                th.setProcessed(false);
                th.clearEvents();
            }
            activeThreads.clear();
        }
    }

    synchronized Iterator<String> getDeviceNames() {
        // make a new set with a copy of the device names, and return an
        // iterator through that.  Otherwise, we would have synchronization
        // issues.
        return new HashSet<String>(deviceMap.keySet()).iterator();
    }

    synchronized AbstractDevice getDevice(final String deviceName) {
        return deviceMap.get(deviceName);
    }

    synchronized void setRandomSeed(final long seed) {
        rand.setSeed(seed);
    }
} // end of class DigitalScheduler

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
