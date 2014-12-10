/*
 * Copyright 2000-2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import com.avlsi.tools.dsim.WakeAt;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.TransactionType;
import com.avlsi.tools.sigscan.Transaction;
import com.avlsi.tools.sigscan.TransactionFiber;
import com.avlsi.tools.sigscan.DebugOpts;
import com.avlsi.tools.sigscan.LoggedString;
import com.avlsi.tools.sigscan.LongAttribute;
import com.avlsi.tools.sigscan.StringAttribute;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.WakeAt;
import com.avlsi.tools.dsim.DSimUtil;

/**
 * <p> Superclass for all devices. </p>
 *
 * <p> Unsure whether necessary; re-examine once more devices implemented. </p>
 *
 * @author Aaron Denney
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 22 July 2002.
 **/

public abstract class AbstractDevice implements Runnable, Startable
{
    // Static Attributes

    /**
     * Class-local debugging.
     **/
    private static final boolean DEBUG = false || TSimDebug.DEBUG;
    private static final boolean VERBOSE = false || TSimDebug.VERBOSE;

    /**
     * Dynamic debugging options for all devices.
     **/
    protected static DebugOpts defaultOpts = new DebugOpts();
    protected final DebugOpts opts;
    
    /**
     * A counter used to generate unique device numbers for the method {@link
     * #getNextDevNumber()}.
     */
    //@ private invariant num_anon_devices >= 0;
    private static int num_anon_devices = 0;

    /**
     * A number used to generate thread IDs.
     **/
    private static AtomicInteger thread_id = new AtomicInteger(0);

    /**
     * Global sigscan to hold the last SigScan built. 
     **/
    public static Sigscan default_sigscan = null;

    /**
     * The three kinds of legal SigScan events.
     **/
    public static final int BEGINEND = Sigscan.KIND_BEGINEND;
    public static final int EVENT = Sigscan.KIND_EVENT;
    public static final int ERROR = Sigscan.KIND_ERROR;

    public static DeviceLogger devLogger = null;
    /**
     * An <code>ArrayList</code> of every <code>AbstractDevice</code> ever
     * constructed so that we can walk them and get time statistics.
     **/
//     private static final ArrayList everyAbstractDevice = new ArrayList();

    /**
     * Map(Thread, String) which maps threads to the names of
     * AbstractDevices.  Every thread which is inside a go() method
     * has an entry here.  The map is weak, so that we don't have a
     * memory leak.  (If a thread dies, we don't need to keep track
     * of it anymore.)
     */
    private static Map threadsInGo =
        Collections.synchronizedMap(new WeakHashMap());

    // Attributes

    /** 
     * Number of "uses" this particular unit has had.  Subclasses should
     * maintain this field. 
     **/
    protected int operations = 0;

    /** 
     * Power consumed.  Suggested units are Watts.  Subclasses should maintain
     * this field. 
     **/
    protected double energy = 0;

    /**
     * Count, in transitions.
     *
     * Subclasses need not maintain this field (i.e. there is no
     * public way to get at it), but it will be handy for debugging
     * purposes, and some of the methods defined here.  (This is no
     * longer true; this is now used to compute Vortex's overall
     * elapsed time at the end of the run)
     **/
    protected long time = 0;

    /**
     * Kludge to let the dispatch be massively parallel.  During
     * parallel blocks, is used for keeping track of the final time of
     * sends and receives so the normal time isn't modified.
     **/
    protected long parallelTime;

    /**
     * Thread currently associated with this device.
     **/
    protected Thread thread;

    /**
     * The name of the scope in which this device was defined (its 'packagename').
     **/
    private String scopename; 

    /**
     * The name of this device.
     **/
    private String name = 
        "No information available -- override getName";

    // SST-related attributes.

    /**This device's SST database**/
    protected Sigscan sigscan=null;
    /**This device's fiber in the SST database **/
    protected TransactionFiber fiber=null;
   
    /**For LOG_MSGS_SST**/
    protected TransactionType output;
    /**For outerr support**/
    protected TransactionType outerr;
    /**For LOG_VARS_SST**/
    protected final LoggedString lstate;
    /**For linking transactions in LOG_CHAN_SST**/
    protected Transaction currTran = null;
    
    /** Transaction types for sends and receives */
    private TransactionType recvTT = null;
    private TransactionType sendTT = null;

    private Transaction lastReceive = null;
    private long lastReceiveTime = -1;

    private StringAttribute recvAttr = null;
    private StringAttribute sendAttr = null;


    // Constructors
   
    /** 
     * Construct an unnamed device.
     **/

    public AbstractDevice() {
        this("Unnamed", "Device"+getNextDevNumber(), defaultOpts);
    }

    /** 
     * Construct a named device with debugging options. 
     *
     * @param fullname name of this device (scopename.name; e.g., vortex.alu0)
     * @param suppressOutput unknown.
     **/

    public AbstractDevice(String fullname, boolean suppressOutput) {
        this(fullname, fullname.lastIndexOf('.'),
             defaultOpts.clone(!suppressOutput));
    }

    /** 
     * Construct a scoped named device with screen debugging option. 
     *
     * @param scopename Scope of this device (ex. vortex)
     * @param name Name of this device (ex. alu0)
     * @param suppressOutput unknown.
     **/

    public AbstractDevice(String scopename, String name, boolean suppressOutput) {
        this(scopename, name, defaultOpts.clone(!suppressOutput)); 
    }

    /** 
     * Construct a scoped named device with full debugging options. 
     *
     * @param scopename Scope of this device (ex. vortex)
     * @param name Name of this device (ex. alu0)
     * @param opts object representing the log options for this device.
     **/

    public AbstractDevice(String scopename, String name, DebugOpts opts) {
        this(scopename, name, default_sigscan, opts);
    }

    /** 
     * Construct a device with SignalScan logging functionality.
     *
     * @param scopename Scope of this device (ex. vortex)
     * @param name Name of this device (ex. alu0)
     * @param sigscan The database file for logging (can be null)
     * @param opts object representing the log options for this device.
     **/

    public AbstractDevice(String scopename, String name, 
                          Sigscan sigscan, DebugOpts opts) {
        this.name = (sigscan == null)?name:
                    name.replace('[','_').replace(']','_');
        this.scopename = (sigscan == null)?scopename:
                         scopename.replace('[','_').replace(']','_');
        
        if (DEBUG) {
            System.err.println(this.name+" => "+name);
            System.err.println("\t"+this.scopename+" => "+scopename);
        }
        this.scopename = scopename;
        this.name = name;
        this.sigscan = sigscan;
        this.opts = opts;
        
        lstate = new LoggedString(this.scopename+"."+this.name,"state",
                                  sigscan, opts, true);

        //SignalScan Stuff
              
        setSigscan(sigscan);

        // Add instance to static table of all instances.
//         everyAbstractDevice.add(this);

        // Running count for statistics.
        if (Statistics.STATISTICS)
            Statistics.incrementDeviceCount();

        if (threadsInGo.containsKey(Thread.currentThread())) {
            String constructee = getFullname();
            String constructor =
                (String)threadsInGo.get(Thread.currentThread());
            tsim3violation("Device " + constructee +
                           " was constructed inside the go() method for " +
                           constructor);
        }
    }

    /** 
     * @todo Undocumented.
     *
     * @param fullname name of this device (scopename.name; e.g., vortex.alu0)
     * @param index unknown.
     * @param opts object representing the log options for this device.
     **/

    private AbstractDevice(String fullname, int index, DebugOpts opts) {
        this(fullname.substring(0,(index !=-1)?index:0),
             fullname.substring(index+1),
             opts);
    }


    // Static Methods

    /**
     * @return the average time of all devices ever created that have non-zero
     * time.
     **/

//     public static synchronized /*@ pure @*/ double meanTimeOfAllDevices() {
//         long totalTime = 0;
//         int deviceCount = 0;

//         for (int i = 0; i < everyAbstractDevice.size(); i++) {
//             AbstractDevice device = (AbstractDevice)everyAbstractDevice.get(i);
//             long deviceTime = device.getTime();
//             // Throw out outliers---devices whose clock have yet to move.
//             if (deviceTime != 0) {
//                 totalTime += deviceTime;
//                 deviceCount++;
//             }
//         }
//         if (deviceCount == 0)
//             return 0.0;
//         else
//             return (double) totalTime / (double) deviceCount;
//     }

//     /**
//      * @return the median time of all devices ever created that have non-zero
//      * time.
//      * @review kiniry - Unimplemented.
//      **/

//     public static synchronized /*@ pure @*/ long medianTimeOfAllDevices() {
//         Debug.unimplemented();
//         return 0;
//     }

//     /**
//      * @return the standard deviation of the time of all devices ever created
//      * that have non-zero time.
//      **/

//     public static synchronized /*@ pure @*/ double 
//         standardDeviationOfTimeOfAllDevices() {
//         return Math.sqrt(varianceOfTimeOfAllDevices());
//     }

//     /**
//      * @return the variance of the time of all devices ever created that have
//      * non-zero time.
//      **/

//     public static synchronized /*@ pure @*/ double varianceOfTimeOfAllDevices() {
//         int deviceCount = 0;
//         double runningSumOfTimeMinusMeanSquared = 0;
//         double meanTime = meanTimeOfAllDevices();

//         if (VERBOSE) {
//             System.err.println("variance meanTime = " + meanTime);
//         }            
//         for (int i = 0; i < everyAbstractDevice.size(); i++) {
//             AbstractDevice device = (AbstractDevice)everyAbstractDevice.get(i);
//             long deviceTime = device.getTime();
//             if (VERBOSE) {
//                 System.err.println("variance deviceTime = " + deviceTime);
//             }
//             // Throw out outliers---devices whose clock have yet to move.
//             if (deviceTime != 0) {
//                 double difference = deviceTime - meanTime;
//                 runningSumOfTimeMinusMeanSquared += difference * difference;
//                 if (VERBOSE) {
//                     System.err.println("variance difference = " + difference);
//                     System.err.println("variance runningSum = " + 
//                                        runningSumOfTimeMinusMeanSquared);
//                 }
//                 deviceCount++;
//             }
//         }
//         if (VERBOSE) {
//             System.err.println("variance count = " + deviceCount);
//         }

//         if (deviceCount == 0)
//             return 0.0;
//         else
//             return runningSumOfTimeMinusMeanSquared / (double) deviceCount;
//     }

    /**
     * Set the default values for the SigScan interface.
     *
     * @todo Undocumented.
     * @param def_sigscan unknown.
     * @param opts unknown.
     *
     * @requires Should there be any preconditions?
     *
     * <pre><jml>
     * private normal_behavior
     *   assignable default_sigscan, defaultOpts;
     *   ensures default_sigscan == def_sigscan;
     *   ensures defaultOpts == opts;
     * </jml></pre>
     **/

    public static void setSigscanDefaults(Sigscan def_sigscan,
                                          DebugOpts opts) {
        Debug.assertTrue(opts != null);
        default_sigscan = def_sigscan;
        defaultOpts = opts;
    }

    /**
     * @return the next unique device number.
     **/
    //@ private normal_behavior
    //@   ensures \result >= 0;
    //@   modifies num_anon_devices;
    private static synchronized int getNextDevNumber() {
        return num_anon_devices++;
    }
    

    // Public Methods
    
    /** Attribute types for sends and receives */

    public DebugOpts getDebugOpts() {
        return opts;
    }

    /**
     * Get full name of the device 
     **/

    public String getFullname() {
        if (scopename.equals(""))
            return name;
        else
            return scopename + "." + name;
    }

    public String getScopename() { return scopename; }

    public int getOperations() {
        return operations;
    }

    public double getEnergy() {
        return energy;
    }

    /**
     * Creates a new thread and starts it.
     *
     * Also adjusts counts on the digital scheduler.
     **/
    public final void start() {
        if (thread != null) return;
        
        thread = new AbstractDeviceThread(thread_id.getAndIncrement(), this,
                                          getFullname());
        //calls incThreadCount for us.
        DigitalScheduler.get().addDevice(this, getFullname());
        thread.start();
    }

    /**
     * Resets and kills thread currently running in this device.
     *
     * To restart it, you'll have to create a new device.
     **/
    public final void interrupt() {
        thread.interrupt();
    }

    /**
     * @return the debugging name of this device.
     **/

    public String getName() {
        return name;
    }

    /**
     * @return a flag indicating if this unit totally idle.  I.e., all input
     * channels empty, not chewing on any instruction, etc.
     **/

    public boolean idle() {
        return true;
    }

    private static boolean waitForReset = true;

    /** XXX - This enables a hack so that prs embedded in tsim will work.  
     * waitForReset seems to have a bug in it -dd
     * This will still work because the real blocks that need to wait for
     * reset are the S2As...
     ***/
    public static void disableWaitForReset() { waitForReset = false; }
    
    /**
     * Public, but shouldn't be called.  For the thread interfaces, basically.
     **/
    // Thread count: start needs to handle incrementing, so it doesn't happen
    // too late, but we can (and should) handle decrementing.
    public final void run() {
        try {
	    if (DSim.activated())
	    	if (waitForReset) waitForReset();
            threadsInGo.put(Thread.currentThread(), getFullname());
            go();
            tsim3violation("Device " + getFullname() +
                           " returned from the go() method");
        } catch (InterruptedException e) {
            if (DEBUG) {
                System.err.println("Interrupting: ");
                e.printStackTrace();
            }
            // interrupts happen during waiting.  The other side was not able to
            // wake us up, and so *probably* hasn't incremented the thread
            // count.
            DigitalScheduler.get().incThreadCount();
            if (DEBUG) {
                System.err.println("thread count bumped.");
            }
        } catch (Throwable t) {
            System.err.println("Caught unexpected throwable: " + t.getMessage());
            System.err.println("in device " + getFullname() +
                    ", of class " + this.toString());
            t.printStackTrace();
        } finally {
            cleanup();
            if (DEBUG) {
                System.err.println("cleanup done: ");
            }
            // We want to do this on normal exit.
            DigitalScheduler.get().decThreadCount();
        }
        stop();
    }

    public final boolean isAlive() {
        if(thread==null) return false; // dead
        return thread.isAlive();
    }

    /** 
     * @todo Undocumented.
     * @exception InterruptedException When?
     **/
    public final void join( long timeOut ) throws InterruptedException {
        if(thread==null) return; // already joined
        thread.join( timeOut );
        thread=null;
    }

    /**
     * Wait until the device's go() exits.
     * @exception InterruptedException When?
     **/
    public final void join() throws InterruptedException {
        if(thread==null) return; // already joined
        thread.join();
        thread=null;
    }
    
    /**
     * @return the (local, relativistic) time of this device.
     *
     * <pre><jml>
     * public normal_behavior
     *   ensures \result >= 0;
     * </jml></pre>
     **/

    public /*@ pure @*/ long getTime() {
        return time;
    }

    /**
     * Helper function to build transaction types.
     *
     * @param kind the kind of transaction, either <code>BEGINEND</code>,
     * </code>EVENT</code> or <code>ERROR</code>.
     * @param hashname the name of this transaction type.  Transaction types
     * with the same name ARE THE SAME OBJECT in SignalScan.
     * @return the new <code>TransactionType</code> object.
     *
     * @see #BEGINEND
     * @see #EVENT
     * @see #ERROR
     *
     * <pre><jml>
     * public normal_behavior
     *   requires kind == BEGINEND || kind == EVENT || kind == ERROR;
     *   ensures \result != null;
     * </jml></pre>
     **/

    public TransactionType newTransactionType(int kind, String hashname) {
        return newTransactionType (this.fiber, kind, hashname);
    }

    /**Begins a new transaction by scheduling a beginTranEvent
     * onto the DigitalSchduler.  This is nec. so that events
     * arrive at the SST in chronological order.  I'm open
     * to alternatives...
     * @param type The type of the Transaction.  Build new
     * transaction types via the default fiber.
     * @param label The label for this new event
     * @param desc The description for this new event
     * @return Returns a Transaction object which can be used
     * to link to other transactions
     **/
    public Transaction beginTransaction(TransactionType type,
                                        String label, String desc) {
        if ((fiber != null) && opts.loggingTrans())
            return beginTransaction (this.fiber, type, label, desc);
        return null;
    }

    /**
     * Ends the most recently begun BEGINEND transaction.  EVENT and ERROR
     * transactions do not need to be ended.
     **/

    public void endTransaction() {
        endTransaction (this.fiber);
    }

    /**Link the message's sender transaction to currTran, a global
     * Transaction**/
    public void link(Message m) { link(m, currTran); }
    
    /**Link the message's sender transaction to the Transaction tran**/
    public void link(Message m, Transaction tran) {
        if ((fiber != null) && opts.loggingTrans()) {
            Transaction sender = m.getSender();
            if (sender != null) {
                //Links in both directions
                link(sender, tran);
            }
        }
    }

    public void link(Transaction t1, Transaction t2) {
        sigscan.link(t1, t2, time);
    }

    /**
     * Get state information.  Override if different behavior desired.
     **/
    public String getState() { return lstate.get(); }
    
    /**Sets the state variable.  If log_vars_sst, then this
     * variable will automatically be logged**/
    public void setState(String state) { /*lstate.set(state,time);*/ }

    /**
     * @return a flag indicating if logging output should be suppressed.
     *
     * @history This method replaces the <code>suppressOutput</code> instance
     * variable.
     **/

    public boolean isOutputSuppressed() {
        return !opts.loggingScreen();
    }
    
    /** 
     * @todo Undocumented.
     * @exception InterruptedException When?
     **/
    public void wait(Clock ck, int edgeType) throws InterruptedException {
        ck.wait(edgeType);
        updateTime(DigitalScheduler.get().getTime());
    }

    /** 
     * @todo Undocumented.
     * @exception InterruptedException When?
     **/
    public void wait(int t) throws InterruptedException {
        delayBy(t);
        new WakeAt(getTime(), new Object()).sleepTil();
    }

    /**
     * Returns the device's SST2 database.
     **/
    public Sigscan getSigscan() {
        return sigscan;
    }

    /**
     * Sets the device's SST2 database.
     **/
    public void setSigscan(final Sigscan sigscan) {
        if (this.sigscan != sigscan) {
            this.sigscan = sigscan;
            initFiber();
        }
    }

    // Protected Methods
    protected void initFiber() {
        fiber = new TransactionFiber(scopename + "." + name, name, sigscan);
        outerr = fiber.newErrorType("outerr");
        output = fiber.newEventType("output");
        recvTT = newTransactionType(BEGINEND, "receive");
        sendTT = newTransactionType(BEGINEND, "send");
        recvAttr = new StringAttribute("data", recvTT,opts);
        sendAttr = new StringAttribute("data", sendTT,opts);
    }

    protected void bumpOperations() {
        operations++;
    }

    protected void addEnergy(double consumed) {
        energy += consumed;
    }

    /**
     * Signals beginning of block in which paralleled calls are made, sets
     * parallel book-keeping time from device time.
     **/
    protected void startParallel() {
        parallelTime = time;
    }

    /**
     * Signals end of block in which paralleled calls are made, sets device time
     * from parallel book-keeping time.
     **/
    protected void endParallel() {
        time = Math.max(time, parallelTime);
    }

    /**
     * Send a message at the current time and update the current time
     * as necessary.
     *
     * @param out the channel on which to send.
     * @param message integer to send.
     * @exception InterruptedException When?
     **/
    protected void send(ChannelOutput out, int message)
            throws InterruptedException {
        send(out, new Message(message, time));
    }

    /**
     * @todo Undocumented.
     * @exception InterruptedException When?
     */
    protected void parallelSend(ChannelOutput out, int message)
            throws InterruptedException {
        parallelSend(out, new Message(message, time));
    }

    /**
     * send a message at the current time and update the current time
     * as necessary
     *
     * @param out the channel on which to send.
     * @param message long to send.
     * @exception InterruptedException When?
     **/
    protected void send(ChannelOutput out, long message)
            throws InterruptedException {
        send(out, new Message(message, time));
    }

    /**
     * Send a message at the current time and update the current time
     * as necessary.
     *
     * @param out the channel on which to send.
     * @param message boolean to send.
     * @exception InterruptedException When?
     **/
    protected void send(ChannelOutput out, boolean message)
            throws InterruptedException {
        send(out, new Message(message, time));
    }

    /**
     * send a message at the current time and update the current time
     * as necessary
     *
     * @param out the channel on which to send.
     * @param message BigInteger to send.
     * @exception InterruptedException When?
     **/
    protected void send(ChannelOutput out, BigInteger message)
            throws InterruptedException {
        send(out, new Message(message, time));
    }

    /**
     * @todo Undocumented.
     * @exception InterruptedException When?
     **/
    protected void parallelSend(ChannelOutput out, BigInteger message)
            throws InterruptedException {
        parallelSend(out, new Message(message, time));
    }

    /**
     * Send a message at the current time and update the current time
     * as necessary.
     *
     * @param out the channel on which to send.
     * @param message message to send.  The time will be the max of
     * the current time and that specified in the message.
     * @exception InterruptedException When?
     **/
    protected void send(ChannelOutput out, Message message)
            throws InterruptedException {
        if (devLogger != null) 
            devLogger.startedSending(this, out,message);
        Transaction tran = null;
        if ((fiber != null) && opts.loggingTrans()) {
            tran = beginTransaction(sendTT, "send", out.toString());
            sendAttr.set(message.toString(),time);
            message.setSender(tran);
            //Crit Path
            if (lastReceive != null) {
               link(lastReceive, tran);
               lastReceive = null;
               lastReceiveTime = -1;
            }
        }
        long newTime = out.send(message, time);
        updateTime(newTime);
        if (tran != null) endTransaction();
        if (devLogger != null) 
                devLogger.endedSending(this, out,message);

        if (opts.loggingScreen()) {
            System.err.println(time + ":" + getCount(out) + getFullname() + ": sent 0x" +
                               message.getValue().toString(16) + " on channel " +
                               out.getName());
        }
    }
    
    /**
     * @return string for number of transactions on channel
     **/     
    protected String getCount(Object out){
        String count = "";
        if (out instanceof BufferedChannel) {
            final BufferedChannel bc = (BufferedChannel) out;
            bc.incCount();
            count = "(" + bc.getCount() + ") "; 
        } else if (out instanceof NodeChannel) {
            Object wrapped = null;
            if (out instanceof NodeReadChannel) {
                wrapped = ((NodeReadChannel) out).unwrap();
            } else if (out instanceof NodeWriteChannel) {
                wrapped = ((NodeWriteChannel) out).unwrap();
            }
            if (wrapped instanceof BufferedNodeChannel) {
                final BufferedNodeChannel nc = (BufferedNodeChannel) wrapped;
                nc.incCount();
                count = "(" + nc.getCount() + ") "; 
            }
        }
        return count;
    }


    /**
     * @todo Undocumented.
     * @exception InterruptedException When?
     **/     
    protected void parallelSend(ChannelOutput out, Message message)
            throws InterruptedException {
        if (opts.loggingTrans() && (currTran != null)) { 
            message.setSender(currTran);
        }
        updateParallelTime(out.send(message, time));
    }

    /**
     * send N messages at the current time and update the current time
     * as necessary
     *
     * @param out the channel on which to send.
     * @param message ints to send.
     * @exception InterruptedException When?
     **/
    protected void send(ChannelOutput [] out, int [] messages)
            throws InterruptedException {
        int l = out.length;
        long [] rv = new long[l];
        Message[] mess = new Message[l];
        Debug.assertTrue(l == messages.length);
        for (int i = 0; i < l; i++) {
            mess[i] = new Message(messages[i], time);
            if ((currTran != null) && opts.loggingTrans())
                mess[i].setSender(currTran);
            rv[i] = out[i].send(mess[i], time);
        }
        updateTime(rv);
    }

    /**
     * send N messages at the current time and update the current time
     * as necessary
     *
     * @param out the channel on which to send.
     * @param message Messages to send.
     * @exception InterruptedException When?
     **/
    protected void send(ChannelOutput [] out, Message [] messages)
            throws InterruptedException {
        int l = out.length;
        long [] rv = new long[l];
        Debug.assertTrue(l == messages.length);
        for (int i = 0; i < l; i++) {
            if ((currTran != null) && opts.loggingTrans())
                messages[i].setSender(currTran);
            rv[i] = out[i].send(messages[i], time);
        }
        updateTime(rv);
    }

    /**
     * If a message is waiting on channel <code>in</code>, return the
     * message without removing it from the channel; otherwise return
     * <code>null</code>.  If a message is waiting, update the device's
     * time to the time at which the message arrived.
     *
     * @param in
     *        The channel whose value is to be probed.
     *
     * <pre><jml>
     * public normal_behavior
     *   assignable time;
     * </jml></pre>
     **/
    public Message probeValue(final /*@ non_null @*/ ChannelInput in) {
        final Message m = in.probeValue();

        if (m != null)
            updateTime(m.getTime());

        return m;
    }

    /**
     * Returns <code>true</code> if a receive on the channel
     * <code>in</code> can complete without blocking at the
     * device's current time.
     *
     * @param in
     *        The channel to probe.
     *
     * <pre><jml>
     * public normal_behavior
     *   assignable time;
     * </jml></pre>
     **/
    public boolean probeReceive(final /*@ non_null @*/ ChannelInput in) {
        return in.probeReceive(getTime());
    }

    /**
     * Returns <code>true</code> if a send on the channel <code>out</code>
     * can complete without blocking at the device's current time.
     *
     * @param out
     *        The channel to probe.
     *
     * <pre><jml>
     * public normal_behavior
     *   assignable time;
     * </jml></pre>
     **/
    public boolean probeSend(final /*@ non_null @*/ ChannelOutput out) {
        return out.probeSend(getTime());
    }

    /**
     * lets devices stall by a certain amount of time.
     **/
    protected void delayBy(int deltaTime) {
        time += deltaTime;
    }

    /**
     * Update local time to the greater of the current time and
     * 'newProspectiveTime'.
     *
     * <pre><jml>
     * normal_behavior
     *   requires newProspectiveTime >= 0;
     *   ensures getTime() == Math.max(\old(time), newProspectiveTime);
     * </jml></pre>
     *
     * @bug kiniry - All mentions of wrappingMax() should be removed.  While
     * longs can still wrap, I guess it <em>would</em> take a hell of a long
     * time...  even simulating a million time units per second it would take
     * over 53 million years...
     **/

    protected void updateTime(long newProspectiveTime) {
        //time = Math.wrappingMax(time, newProspectiveTime);
        time = Math.max(time, newProspectiveTime);
    }

    /**
     * Update local "parallel" time to the greater of the current parallel time
     * and 'newProspectiveTime'.
     *
     * <pre><jml>
     * normal_behavior
     *   requires newProspectiveTime >= 0;
     *   ensures getTime() == Math.max(\old(time), newProspectiveTime);
     * </jml></pre>
     *
     * @bug kiniry - All mentions of wrappingMax() should be removed.  While
     * longs can still wrap, I guess it <em>would</em> take a hell of a long
     * time...  even simulating a million time units per second it would take
     * over 53 million years...
     **/

    protected void updateParallelTime(long newProspectiveTime) {
        parallelTime = Math.max(parallelTime, newProspectiveTime);
    }

    /**
     * Receive a message from channel 'in'.  If no message is available, block
     * until one is.
     *
     * @param in the channel on which to perform the receive.
     * @exception InterruptedException the current thread is interrupted.
     * @return the received message.
     *
     * <pre><jml>
     * normal_behavior
     *   requires in != null;
     * </jml></pre>
     **/

    protected Message receive(ChannelInput in) throws InterruptedException {
        if (devLogger != null)
            devLogger.startedReceiving(this,in);
        Transaction tran = null;
        if (recvTT != null)
            tran = beginTransaction(recvTT, "receive", in.toString());
        Message m = in.receive(time);
        updateTime(m.getTime());
        if (recvTT != null) {
            recvAttr.set(m.toString(),time);
            endTransaction();
            link(m, tran);
            if ((time - lastReceiveTime) > 0) {
               //Updating the last receive transaction for crit path
                lastReceiveTime = time;
                lastReceive = tran;
            }
        }

        if (opts.loggingScreen()) {
            System.err.println(time + ": " + getCount(in) + getFullname() +
                    ": received 0x" + m.getValue().toString(16) +
                    " on channel " + in.getName());
        }
        if (devLogger != null)
            devLogger.endedReceiving(this,in,m);
        return m;
    }

    /**
     * Receive messages from an array of channels 'ins'.  The result array is
     * pairwise matched with the channels.  E.g., result[0] is the message that
     * was received on channel ins[0].
     *
     * @param ins the channels on which to perform the receive.
     * @exception InterruptedException the current thread is interrupted.
     * @return an array of received messages.
     *
     * <pre><jml>
     * normal_behavior
     *   requires ins != null;
     *   requires (\forall int i; i <= 0 && i < ins.length; ins[i] != null);
     *   ensures \result.length == ins.length;
     * </jml></pre>
     **/

    protected Message [] receive(ChannelInput [] ins) throws InterruptedException {
        int l = ins.length;
        Message rv [] = new Message [l];
        long times [] = new long [l];
        for (int i = 0; i < l; i++) {
            rv[i] = ins[i].receive(time);
            times[i] = rv[i].getTime();
        }  
        // Update the time of the device based upon the set of all received
        // messsages.
        updateTime(times);
        return rv;
    }

    /**
     * Print some logging information with a standard header.  If this device is
     * connected to SignalScan, the information is sent via a fiber transaction,
     * otherwise it is sent to <code>System.out</code>.
     *
     * @param text the message to log.
     * @see fiber
     **/

    protected void output(String text) {
        if ((fiber != null) && opts.loggingMsgs() && (thread != null)) {
            // @review kiniry - Why is this commented out?
            //fiber.scheduleEvent(new BeginTranEvent(sigscan, fiber, time,
            //    output, "output", text));
            fiber.beginTransaction(output, "output", text, time);
        }
        if (opts.loggingScreen()) {
            System.err.println(getTime() + ": " + getFullname() + ": " + text);
            System.err.flush();
        }
    }

    /**
     * Print some logging information to <code>System.err</code> with a standard
     * header.
     *
     * @param text the message to log.
     **/

    protected void outerr(String text) {
        System.err.println(time + ": " + getFullname() + ": " + text);
        System.err.flush();
        if ((sigscan != null) && (fiber != null)&& (thread != null)) {
            fiber.beginTransaction(outerr, "outerr", text,time);
        }
    }

    /**
     * Adds this abstract device to the list of named abstract devices. 
     * The device will be removed after <code>cleanup()</code> is called.
     *
     * @deprecated  There is no longer a static list of devices, just
     *   delete the call to addDevice.
     **/

    protected void addDevice(String name) {
    }

    /**
     * Implement this instead of <code>run()</code>.
     *
     * @todo Undocumented.
     * @exception InterruptedException the current thread is interrupted.
     **/

    protected abstract void go() throws InterruptedException;

    /**
     * Override this as an exit function for the end of a simulation.
     **/

    protected void stop() {
	return;
    }

    /**
     * Cleanup when an <code>InterruptedException</code> occurs.  This method is
     * typically used to destroy channels.
     **/

    protected void cleanup() {
    }

    /** 
     * Same as public method, but takes an argument for the SST fiber.
     *
     * @param fiber a fiber in the SST database.
     * @param type the type of the transaction.  Build new transaction types via
     * the default fiber.
     * @param label the label for this new event.
     * @param desc the description for this new event.
     * @return a transaction object which can be used to link to other
     * transactions.
     *
     * @see #beginTransaction(TransactionType, String, String)
     * @review kiniry - This doesn't look implemented fully yet.
     **/

    protected Transaction beginTransaction(TransactionFiber fiber, 
                                           TransactionType type,
                                           String label, String desc) {
        if ((fiber != null) && opts.loggingTrans()) {
            //return fiber.scheduleBeginTranEvent(
            //        new BeginTranEvent(sigscan, fiber, time,
            //                           type, label, desc));
            return fiber.beginTransaction(type, label, desc,time);
        }
        return null;
    }

    /**
     * Helper function to build transaction types.
     *
     * @param fiber a fiber in the SST database.
     * @param kind the kind of transaction, either <code>BEGINEND</code>,
     * </code>EVENT</code> or <code>ERROR</code>.
     * @param hashname the name of this transaction type.  Transaction types
     * with the same name ARE THE SAME OBJECT in SignalScan.
     * @return the new <code>TransactionType</code> object.
     *
     * @see #BEGINEND
     * @see #EVENT
     * @see #ERROR
     *
     * <pre><jml>
     * normal_behavior
     *   requires kind == BEGINEND || kind == EVENT || kind == ERROR;
     *   ensures \result != null;
     * </jml></pre>
     **/

    protected TransactionType newTransactionType(TransactionFiber fiber, 
                                                 int kind, String hashname) {
        if (fiber != null) 
            return fiber.newTransactionType(hashname,kind);
        return null;
    }
    
    /**
     * Ends the most recently begun BEGINEND transaction on the specified fiber.
     * EVENT and ERROR transactions do not need to be ended.
     *
     * @param fiber a fiber in the SST database.
     **/

    protected void endTransaction(TransactionFiber fiber) {
        if ((fiber != null) && opts.loggingTrans()) {
            //fiber.scheduleEvent(new EndTranEvent(sigscan, fiber, time));
            fiber.endTransaction(time);
        }
        currTran = null;
    }

    // Private Methods

    /**
     * Adjust this device's time up to maximum of an array of times.
     * 
     * @param times an array of times from which to update.
     *
     * <pre><jml>
     * private normal_behavior
     *   requires times.length > 0 ==>
     *            (\forall int i; i <= 0 && i < times.length; times[i] >= 0);
     *   assignable time;
     *   ensures (\forall int i; i <= 0 && i < times.length; times[i] <= time);
     * </jml></pre>
     **/

    private void updateTime(long [] times) {
        long maxtime = time;

        for (int i = 0; i < times.length; i++) {
            maxtime = Math.max(times[i], maxtime);
        }

        time = Math.max(time, maxtime);
    }

    /** 
     * When cosimulating, wait until our DSim has reset itself.
     *
     * @exception InterruptedException the current thread is interrupted.
     *
     * @review kiniry - Why do all this complicated stuff with Wait when we can
     * just use <code>Thread.currentThread().sleep()</code>?
     **/

    protected static boolean outWarn = false;
    protected Object warnLock = new Object();
    
    protected void waitForReset() throws InterruptedException {
        Node resetNode = DSimUtil.getResetNode();
        if (resetNode == null) {
            synchronized(warnLock) {
                if (!outWarn) {
                    System.err.println("Warning: No reset node was found.");
                    outWarn = true;
                }
            }
            return;
        }
        if (DEBUG) {
            System.err.println(getFullname()+" Sleeping til PReset + 00");
        }
        Wait wait = new Wait(null, null, new Node[]{resetNode}, null);
        wait.select();
        if (DEBUG) {
            System.err.println(getFullname()+" Starting Awakening.  Time is " + DSim.get().getTime());

        }
        new WakeAt(DSim.get().getTime() + 00, this).sleepTil();
        if (DEBUG) {
            System.err.println(getFullname()+" PReset:  Awakening.  Time is " + DSim.get().getTime());
        }
        return;
    }

    /**
     * Print a warning message about something which will not be
     * compatible with tsim3.
     */
    private static synchronized void tsim3violation(String msg) {
        System.err.println(StringUtil.repeatString("v", 70));
        System.err.println("Warning: This will be illegal in tsim3:");
        System.err.println(msg);
        System.err.println(StringUtil.repeatString("^", 70));
    }

    public synchronized Node getEnablingNode() {
        return null;
    }

    public synchronized void setEnablingNode(final Node node) {
    }

} // end of abstract class AbstractDevice

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
