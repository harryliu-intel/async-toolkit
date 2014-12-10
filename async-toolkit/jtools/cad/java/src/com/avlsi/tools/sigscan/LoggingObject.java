/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for automatically logging objects
 * Provides an interface to a default fiber, a default state string
 * which is logged, and holds object name information.
 *
 * @author Dan Daly
 * @version $Date$
 **/

public abstract class LoggingObject {

    //
    //TransactionType Kinds
    //
    
    /** Transaction Kind for a transaction that has a begin and an end**/
    public static final int BEGINEND = Sigscan.KIND_BEGINEND;
    
    /** Instantaneous Transaction **/
    public static final int EVENT = Sigscan.KIND_EVENT;
    
    /** Instantaneous Transaction that will increment the global
     * error count and draw a red dot in the viewer on the fiber
     **/
    public static final int ERROR = Sigscan.KIND_ERROR;
    
    /**The full name of this object**/
    private final String fullname;
    
    /**The scope to place this object in the database **/
    private final String scopename;

    /**The name of this object **/
    private final String name;

    /**The database this object resides in **/
    protected final Sigscan sigscan;
   
    /**The Debug Options of this object **/
    protected final DebugOpts opts;
    
    /** The state variable **/
    private final LoggedString lstate;

    /** The default fiber **/
    protected TransactionFiber fiber=null;
    
    /**For the output() function**/
    protected TransactionType output=null;
    
    /**For the outerr() function**/
    protected TransactionType outerr=null;

    /**Used to see if there is an open transaction **/
    protected Transaction currTran=null;
    
    /**
     * Constructs a LoggingObject.  Will build the default fiber, and the
     * default state variable.
     * @param fullname The fully scoped name of this object
     * @param sigscan The database to place this in (can be null)
     * @param opts The database options
     **/
    public LoggingObject(String fullname, Sigscan sigscan, DebugOpts opts) {
        this(fullname, fullname.lastIndexOf('.'), sigscan, opts);
    }

    private LoggingObject(String fullname, int index,
                          Sigscan sigscan,DebugOpts opts) {
        this(fullname.substring(0,(index !=-1)?index:0),
             fullname.substring(index+1),
             sigscan, opts);
    }

    /**
     * Constructs a LoggingObject.  Will build the default fiber, and the
     * default state variable.
     * @param scopename The scope to place this object in
     * @param name The name of this object
     * @param sigscan The database to place this in (can be null)
     * @param opts The database options
     **/
    public LoggingObject(String scopename, String name,
                         Sigscan sigscan, DebugOpts opts) {
        this.fullname = scopename+"."+name;
        this.scopename = scopename;
        this.name = name;
        this.sigscan = sigscan;
        this.opts = opts;

        lstate = new LoggedString(getFullname(), "state", sigscan, opts);

        if (sigscan != null) {
            fiber = new TransactionFiber(getFullname(), getName(), sigscan);
            outerr = fiber.newErrorType("outerr");
            output = fiber.newEventType("output");
        }
    }

    /**@return The object's fully scoped name. **/
    public String getFullname() { return fullname; }

    /**@return The object scopename.  This is where the object can be found
     * in the database.
     **/
    public String getScopename() { return scopename; }
    
    /**@return The object name.  Does not return the fully qualified name,
     * use getFullname() to get a fully scoped name.
     **/
    public /*@ pure @*/ String getName() { return name; }

    /** @return The state of the device (if the device keeps track of
     * its state)
     **/
    public String getState() { return lstate.get(); }

    /**
     * @param str The new state of the device
     **/
    public void setState(String str) { 
        if ((sigscan != null) && opts.loggingSST()) setTime();
        lstate.set(str);
    }

    /**@return the DebugOpts of this object **/
    public DebugOpts getDebugOpts() { return opts; }

    /** @return The current time of the simulation according to the device.
     * Implementation Specific
     **/
    protected abstract long getTime();

    /**Reports the time to sigscan before doing a logging event
     * Assumes that we're logging**/
    protected void setTime() {
        if (//(sigscan != null) &&
            //opts.loggingSST() &&
            (sigscan.getTimeInCustomUnits() < getTime())) {
            try {
                sigscan.convertAndSetTime(getTime());
            } catch (SigscanException e) {
                fiber.setTimeError(e,getTime());
            }
        }
    }

    /**
     * Println with a standard header.  If loggingMsgs(), then
     * will also log a message to the default fiber.
     **/
    protected void output(String text) {
        if ((fiber != null) && opts.loggingMsgs()){
            setTime();
            fiber.beginTransaction( output, "output", text,getTime());
        }
        if (opts.loggingScreen()) {
            System.out.println(getTime() + ": " + getFullname() + ": " + text);
            System.out.flush();
        }
    }

    /**
     * Println to stderr with a standard header.  If logging is
     * enabled, will write an error onto the default fiber.
     **/
    protected void outerr(String text) {
        System.err.println(getTime() + ": " + getFullname() + ": " + text);
        System.err.flush();
        if ((sigscan != null) && (fiber != null)) {
            setTime();
            fiber.beginTransaction(outerr, "outerr", text,getTime());
        }
    }
    
    /**Helper function to build transaction types
     * @param kind the kind of transaction, BEGINEND, EVENT or ERROR
     * @param hashname the name of this transaction type.  Transaction
     * types with the same name ARE THE SAME OBJECT in signalscan
     * @return the TransactionType object**/
    protected TransactionType newTransactionType(int kind, String hashname) {
        if (fiber != null) 
            return fiber.newTransactionType(hashname,kind);
        return null;
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
    protected Transaction beginTransaction(TransactionType type,
                                 String label, String desc) {
        if ((fiber!=null) && opts.loggingTrans()) {
            setTime();
            currTran = fiber.beginTransaction( type, label, desc,getTime());
            return currTran;
        }
        return null;
    }

    /**Ends the most recently begun BEGINEND transaction.  EVENT and
     * ERROR transactions do not need to be ended.**/
    protected void endTransaction() {
        if ((fiber != null) && (currTran != null)) {
            setTime();
            fiber.endTransaction(getTime());
        }
        currTran = null;
    } 
    
    /** Will link transactions in both directions, predecessor and
     * successor.
     * @param firstTran The first transaction in time, will become 
     * <code>secondTran's</code> predecessor.
     * @param secondTran The second transaction in time, will become
     * <code>firstTran's</code> successor.
     **/
    protected void link(Transaction firstTran, Transaction secondTran) {
        if ((sigscan != null) && (firstTran != null) && (secondTran != null))
            sigscan.link(firstTran, secondTran,getTime());
    }
    
    /** Will link transactions in one direction, linking the src with its
     * predecessor.
     * @param tran The second transaction in time, to be related to
     * <code>predecessor</code>
     * @param predecessor The first transaction in time, will become
     * <code>tran's</code> predecessor.
     **/
    protected void linkPredecessor(Transaction tran,Transaction predecessor) {
        if ((sigscan != null) && (predecessor != null) && (tran != null))
            sigscan.linkPredecessor(tran,predecessor,getTime());
    }

    /** Will link transactions in one direction, linking the src with its
     * predecessor.
     * @param tran The second transaction in time, to be related to
     * <code>successor</code>
     * @param successor The first transaction in time, will become
     * <code>tran's</code> successor.
     **/
    protected void linkSuccessor(Transaction tran,Transaction successor) {
        if ((sigscan != null) && (successor != null) && (tran != null))
            sigscan.linkSuccessor(tran,successor,getTime());
    }

}

