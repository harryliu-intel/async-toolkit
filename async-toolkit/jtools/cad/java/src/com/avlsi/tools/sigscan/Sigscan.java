/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.tools.sigscan;
import com.avlsi.tools.aspice.AnalogTraceInterface;
import com.avlsi.file.common.HierName;
import java.util.Iterator;
/**
 * Class that encapsulated all native calls to sigscan<BR>
 * Note that this class uses native methods.  All native calls are 
 * static because all instance info is passedto the calls.
 * Also, multiple Sigscan classes can use the same library.  In most cases
 * you don't want to call Sigscan directly, but use the Java classes that
 * encapsulate these static methods.
 * @author Dan Daly
 * @version $Date$
 **/

public class Sigscan {

    private static final Object APIlock = new Object();

    /*Records the most recently instantiated database*/
    private static Sigscan mostRecent=null;
    
    /*Handle for the database*/
    protected final long sdiHandle;

    /** Name of the database */
    protected final String name;

    /* Handle for predecessor linker **/
    protected final long plink;

    /** Handle for successor linker */
    protected final long slink;

    /* timeScale of database which is the smallest unit of time
     * equal to 10 ^ timeScale **/
    protected final int timeScale;
    
    /**This is the conversion between database time and the simulator
     * time **/
    protected double sigscanConversion=1;
    
    private static boolean libloaded = false;
    
    /*Loads the native library*/
    private static native long openDatabase(String name,
            boolean useOpen, int timeScale) throws SigscanException;

    /** Pause the database to flush transactions. **/
    private static native void pauseDatabase(long sdiHandle);

    /** Call after pauseDatabase to resume. **/
    private static native void resumeDatabase(long sdiHandle);

    /**Loads the library if not alraedy loaded **/
    private static void loadLib() throws SigscanException {
        if (!libloaded) {
            try {
               System.loadLibrary("Sigscan");
               libloaded = true;
            } catch (UnsatisfiedLinkError e) {
                throw new SigscanException(
                                    "Unsatisfied Link Error with loading"+
                                    " signal scan native library: "+
                                    e.getMessage(), e);
            }
        }
    }

    /*Set the time of the database with handle sdiHandle**/
    private static native void setTime(long sdiHandle, long time);

    /*Local tally of the current time according to the database*/
    private long currTime = 0;
    
    /**  Sets the current database time to <code>time</code>
     * the database's time units.  Database
     * entries must be entered in chronological order 
     * @param time The new time of the database
     * @throws SigscanException If the new time is less than the
     * current database time, an error is thrown. **/
    public void setTime(long time) throws SigscanException {
        if (time > currTime) {
            
            synchronized(APIlock) { if (open) setTime(sdiHandle, time); }
            currTime = time;
        } else if (time < currTime) {
            throw new SigscanException("Error, sigscan set time "+time+
                               " is in the past,  using current time "+
                               currTime);
        }
    }

    /**
     * Gets the current record time in database time units.
     * @return the current time in database units
     **/
    public long getTime() {
        return currTime;
    }

    /** Gets the current record time in the simulation's own time
     * units.
     * @see setSigscanConversion
     * @return The current time in the simulation's units (not the database
     * units)
     **/
    public long getTimeInCustomUnits() {
        return getCustomUnits(getTime());
    }
    
    private Transaction lastError = null;
    private Object errorLock = new Object();
    private int errorCount = 0;

    public void incErrorCount() { errorCount++; }

    /* Function used to log error messages to the database
     * when a timing error is found.
     * @param tran The transaction that caused the timing error
     **/
    public void reportError(Transaction tran) {
        synchronized(errorLock) {
            if (lastError != null) {
                link(lastError, tran,getTime());
            }
            tran = lastError;
        }
        incErrorCount();
    }
        
    /**Use this to have Sigscan convert from your custom units to the
     * units of the database.  Call convertAndSetTime instead of setTime.  
     * When calculating <code>conv</code> be 
     * sure to consider the database's timeScale.
     * @param conv Conversion factor between sigscan units and your units.  Thus conv = #sigscan units / #your units
     * @see #getTimeScale()
     * @see #convertAndSetTime(long)
     **/
    public void setSigscanConversion(double conv) { sigscanConversion = conv; }

    /**
     * Convert from database time units to your custom time units.
     **/
    public long getCustomUnits(long ssu) {
        return (Math.round(ssu / sigscanConversion));
    }

    /** Sets the time to yourUnits*sigscanConversion, assumes that 
     * sigscanConversion was correctly set with setSigscanConversion.
     * 
     * @param yourUnits The time to log in your simulator's units
     * @throws SigscanException if the time sent to database is in the past
     ***/
    public void convertAndSetTime(long yourUnits) throws SigscanException{
        setTime(Math.round(yourUnits*sigscanConversion));
    }
   
    /** @return The universal time of the java simulator **/
    private long getSimTime() {
        return com.avlsi.tools.dsim.DigitalScheduler.get().getTime();
    }

    private void addEvent(LogEvent e) {
        //com.avlsi.tools.dsim.DigitalScheduler.get().addEvent(e);
        e.add();
    }

    /** deletes the database natively, the database will not be 
     * readable until you close it **/
    private static native void closeDatabase(long sdiHandle);
   
    /*Frees a native handle*/
    private static native void freeHandle(long handle);

    /** Frees that chunk of memory residing natively.  Please use
     * with caution.
     * @param handle The handle of the object to free.
     ***/
    public static void freeHandle(ObjectHandle handle) {
        if (handle.getHandle() != 0)
            freeHandle(handle.getHandle()); }
    
    private boolean open = false;
    
    /** Pause to flush the database and make it viewable. **/
    public void pause() {
        if (open) {
            synchronized(APIlock) {
                pauseDatabase(sdiHandle);
            }
        }
    }

    /** Resume to continue recording after a pause. **/
    public void resume() {
        if (open) {
            synchronized(APIlock) {
                resumeDatabase(sdiHandle);
            }
        }
    }
    
    /** The database MUST be closed in order for the data to be accessed. **/
    private Thread cleanupThread = null;

    public void close() { 
        if (open) {
            System.out.print("[Sigscan "+name+"] Total Timing Errors ("+errorCount+
                             "), Trying to close database...");
            synchronized(APIlock) {
                open = false;
                closeDatabase(sdiHandle);
                if (cleanupThread != null &&
                    Thread.currentThread() != cleanupThread) {
                    Runtime.getRuntime().removeShutdownHook(cleanupThread);
                    cleanupThread = null;
                }
            }
            System.out.println("done.");
        }
    }
    //
    //Int and String Functionality to record Signals
    //

    private static native long newIntVariable(long sdiHandle, 
                                               String scopename, 
                                               String varname);
    
    /**Builds a new integer variable.  This is a signal in the database.  
     * Note, that this probably shouldn't be called directly.
     * @param scopename Scope of the variable 
     * (where the variable is found in the Design Browser window).
     * @param varname The name of this integer variable
     * @return A native address to the new variable
     * @see com.avlsi.tools.sigscan.LoggedInt
     **/
    public long newIntVariable(String scopename, String varname) {
        long ptr = 0;
        synchronized(APIlock) {
            if (open)
                ptr = newIntVariable(sdiHandle, scopename, varname);
        }
        return ptr;
    }
    
    private static native void recordIntChange(long varHandle, int data);

    /**Record a change in an integer variable.  Be sure that the
     * database time is set correctly before calling this function.
     * Probably shouldn't be called directly.
     * @param handle The C address of the variable
     * @param date The new value
     * @see com.avlsi.tools.sigscan.LoggedInt
     **/
    public void intChange(long handle, int data) {
        synchronized(APIlock) {
            if (open) recordIntChange(handle, data); 
        }
    }
    
    private static native long newLongVariable(long sdiHandle, 
                                               String scopename, 
                                               String varname);
    
    /**Builds a new long variable.  This is a signal in the database.  
     * Note, that this probably shouldn't be called directly.
     * @param scopename Scope of the variable
     * (where the variable is found in the Design Browser window).
     * @param varname The name of this long variable
     * @return A native address to the new variable
     * @see com.avlsi.tools.sigscan.LoggedLong
     **/
    public long newLongVariable(String scopename, String varname) {
        long ptr =0;
        synchronized(APIlock) {
            if (open) ptr = newLongVariable(sdiHandle, scopename, varname);
        }
        return ptr;
    }
    
    private static native void recordLongChange(long varHandle, long data);
    
    /**records a change in the long variable.  Be sure that the
     * database time is set correctly before calling this function.
     * Probably shouldn't be called directly.
     * @param varHandle C pointer returned from newLongVariable
     * @param data the new value
     * @see com.avlsi.tools.sigscan.LoggedLong
     **/
    public void longChange(long varHandle, long data) {
        synchronized(APIlock) { if (open) recordLongChange(varHandle,data); }
    }
    
    private static native long newStringVariable(long sdiHandle, 
                                                  String scopename, 
                                                  String varname);
    
    /**Builds a new string variable.  This is a signal in the database.  
     * Note, that this probably shouldn't be called directly.
     * @param scopename Scope of the variable 
     * (where the variable is found in the Design Browser window).
     * @param varname The name of this string variable
     * @return A native address to the new variable
     * @see com.avlsi.tools.sigscan.LoggedString
     **/
    public long newStringVariable( String scopename, String varname) {
        long ptr = 0;
        synchronized(APIlock) {
            if (open) ptr = newStringVariable(sdiHandle, scopename, varname);
        }
        return ptr;
    }
    
    /**records a change in the string variable.  Be sure that the
     * database time is set correctly before calling this function.
     * Probably shouldn't be called directly.
     * @param varHandle C pointer returned from newStringVariable
     * @param data the new value
     * @see com.avlsi.tools.sigscan.LoggedString
     **/
    public void stringChange(long varHandle, String data) {
        synchronized(APIlock) { if (open) recordStringChange(varHandle, data); }
    }
        
    private static native void recordStringChange(long varHandle, String data);

    //
    //Double Functionality (ie for Analog Signals)
    //
    
    private static native long newDoubleVariable(long sdiHandle, 
                                                  String scopename, 
                                                  String varname);
    
    /**Builds a new double variable.  This is a signal in the database.  
     * Note, that this probably shouldn't be called directly.
     * @param scopename Scope of the variable 
     * where the variable is found in the Design Browser window.
     * @param varname The name of this double variable
     * @return A native address to the new variable
     * @see com.avlsi.tools.sigscan.LoggedDouble
     * @see com.avlsi.tools.sigscan.AnalogTrace
     **/
    public long newDoubleVariable(String scopename, String varname) {
        long ptr = 0;
        synchronized(APIlock) {
            if (open)
                ptr = newDoubleVariable(sdiHandle, scopename, varname);
        }
        return ptr;
    }
    
    /**records a change in the double variable.  Be sure that the
     * database time is set correctly before calling this function.
     * Probably shouldn't be called directly.
     * @param varHandle C address returned from newDoubleVariable
     * @param data the new value
     * @see com.avlsi.tools.sigscan.LoggedDouble
     **/
    public void doubleChange(long varHandle, double data) {
        synchronized(APIlock) {
            if (open) recordDoubleChange(varHandle, data);
        }
    }
    
    private static native void recordDoubleChange(long varHandle, double data);

    //
    //Special Asynchronous Signal Functionality for Async Digital Signals
    //
    
    /*The value of a ZERO (0) on an signal line*/
    public static final int VALUE_0 = com.avlsi.tools.dsim.Node.VALUE_0;
    
    /*The value of a ONE (1) on an signal line*/
    public static final int VALUE_1 = com.avlsi.tools.dsim.Node.VALUE_1;
    
    /*The value of an unstab (U) on an signal line*/
    public static final int VALUE_U = com.avlsi.tools.dsim.Node.VALUE_U;

    /*The list of signals names in our Async Enumeration*/
    public static final String[] sig_names = {"0", "1", "U" };

    /** Synchronizes the signal state enumeration between Java and SST **/
    private static native void setAsyncEnumeration(String[] names);

    private boolean enum_set = false;

    /** Generates a new variable in C, returns the handle of the var **/
    private static native long newAsyncVariable(long sdiHandle, 
                                                 String scopename, 
                                                 String varname);

    /**Builds a new async 0/1/U variable.
     * This is an enumerated type of signals
     * in the database.
     * Note, that this probably shouldn't be called directly.
     * @param scopename Scope of the variable 
     * (where the variable is found in the Design Browser window).
     * @param varname The name of this async variable
     * @return A native address to the new variable
     * @see com.avlsi.tools.dsim.DSim
     **/
    public long newAsyncVariable(String scopename, String varname){
        if (!enum_set) {
            setAsyncEnumeration(sig_names);
            enum_set = true;
        }
        long ptr =0;
        synchronized(APIlock) {
            if (open) ptr = newAsyncVariable(sdiHandle, scopename, varname); 
        }
        return ptr;
    }

    /**records a change in the async variable.  Be sure that the
     * database time is set correctly before calling this function.
     * Probably shouldn't be called directly.
     * @param varHandle A native address returned from newAsyncVariable
     * @param data the new value
     * @see com.avlsi.tools.dsim.DSim
     **/
    public void asyncChange(long varHandle, int value) {
        synchronized(APIlock) { if (open) recordAsyncChange(varHandle, value); }
    }
    
    private static native void recordAsyncChange(long varHandle, int value);
  
    //
    //Synchronous Signal Functionality, std logic 0/1/X/Z
    //

    public static final int LOGIC0 = 0; //DO NOT change these #s
    public static final int LOGIC1 = 1; //Must match w/ native code in
    public static final int LOGICX = 2; //sdi2.h
    public static final int LOGICZ = 3;
    
    /** Generates a new variable in C, returns the handle of the var **/
    private static native long newLogicVariable(long sdiHandle, 
                                                 String scopename, 
                                                 String varname);

    /**Builds a new logic 0/1/X/Z variable.  This is a signal
     * in the database.  the viewers will also display X and Z in
     * different colors compared to 0 and 1 (yee haw).
     * Note, that this probably shouldn't be called directly.
     * @param scopename Scope of the variable 
     * (where the variable is found in the Design Browser window).
     * @param varname The name of this async variable
     * @return A native address to the new variable
     * @see com.avlsi.tools.dsim.DSim
     **/
    public long newLogicVariable(String scopename, 
                                String varname) {
        long ptr = 0;
        synchronized(APIlock) {
            if (open) ptr = newLogicVariable(sdiHandle,scopename, varname);
        }
        return ptr;
    }

    /**records a change in the logic variable.  Be sure that the
     * database time is set correctly before calling this function.
     * Probably shouldn't be called directly.
     * @param varHandle A native address returned from newLogicVariable
     * @param data the new value
     * @see com.avlsi.tools.dsim.DSim
     **/
    public void logicChange(long varHandle, int value) {
        synchronized(APIlock) { if (open) recordLogicChange(varHandle, value); }
    }
    
    private static native void recordLogicChange(long varHandle, int value);
    
    /** Generates a new variable in C, returns the handle of the var **/
    private static native long newLogicArrayVariable(long sdiHandle, 
                                                     String scopename, 
                                                     String varname,
                                                     int lsb, int msb);

    /**Builds a new logic 0/1/X/Z array variable.  This is a signal
     * in the database.  the viewers will also display X and Z in
     * different colors compared to 0 and 1 (yee haw).
     * Note, that this probably shouldn't be called directly.
     * @param scopename Scope of the variable 
     * (where the variable is found in the Design Browser window).
     * @param varname The name of this logic array variable
     * @return A native address to the new variable
     * @see com.avlsi.tools.sigscan.LoggedLogicArray
     * @see com.avlsi.tools.dsim.DSim
     **/
    public long newLogicArrayVariable(String scopename, 
                                      String varname,
                                      int lsb, int msb) {
        long ptr = 0;
        synchronized(APIlock) {
            if (open) ptr = newLogicArrayVariable(sdiHandle,scopename,varname,
                                                  lsb, msb);
        }
        return ptr;
    }

    /**records a change in the logic array variable.  Be sure that the
     * database time is set correctly before calling this function.
     * Probably shouldn't be called directly.
     * @param varHandle A native address returned from newLogicVariable
     * @param data the new value
     * @see com.avlsi.tools.dsim.DSim
     **/
    public void logicArrayChange(long varHandle, byte[] value) {
        synchronized(APIlock) {
            if (open) recordLogicArrayChange(varHandle, value);
        }
    }
    
    private static native void recordLogicArrayChange(long varHandle,
                                                      byte[] value);
    
    //
    //Transaction Functionality
    //

    private static native long newTransactionFiber(long sdiHandle, 
                                                    String scopename, 
                                                    String fibername);

    /** Builds a new transaction fiber, and returns the handle to that
     * fiber. Do not call, use TransactionFiber constructor instead  
     * @param scopename The scope where this fiber lies
     * @param fibername The name of this fiber
     * @see com.avlsi.tools.sigscan.TransactionFiber
     ***/
    long PRIVATEnewTransactionFiber(String scopename, String fibername) {
        long ptr = 0;
        synchronized(APIlock) {
            if (open)
                ptr = newTransactionFiber(sdiHandle, scopename, fibername);
        }
        return ptr;
    }
    
    //Different kinds of transactions DO NOT CHANGE their values
    //Hack:  I've hardcoded the values in C for now, should use
    //accessor function (like how I did it for Asyncs)

    /*This kind has a definite begin and end */
    public static final int KIND_BEGINEND = 1;
    /*This kind shows up as an error, and is instant */
    public static final int KIND_EVENT = 2;
    /*Another kind of instant transaction, signifying an event */
    public static final int KIND_ERROR = 3;
    
    private static native long newTransactionType(long fiberHandle,
                                                   String typename,
                                                   int trankind);

    /**Builds a new transaction type, and returns its handle. Probably
     * shouldn't be called directly
     * @param typename The name of the TransactionType
     * @param trankind KIND_BEGINEND, KIND_EVENT, etc
     * @param fiber The fiber that this type will be on
     * @see com.avlsi.tools.sigscan.TransactionType **/
    long PRIVATEnewTransactionType(
            String typename, int trankind, TransactionFiber fiber) {
        long ptr = 0;
        synchronized (APIlock) {
            if (open)
                ptr = newTransactionType(fiber.getHandle(), 
                                         typename, trankind);
        }
        return ptr;
    }

    private static native long beginTransaction(long fiberHandle,
                                                long tranTypeHandle,
                                                 String label,
                                                 String description);

    /** Begins a new transaction of type <code>type</code>.
     * @param fiber The fiber to start the transaction on (cannot be null).
     * @param type The type of Transaction (cannot be null)
     * @param label The label to put on this transaction
     * @param desc The description of this transaction
     * @return The transaction that was just started
     * @see com.avlsi.tools.sigscan.TransactionFiber**/
    Transaction beginTransaction(
            TransactionFiber fiber, TransactionType type,
            String label, String description) {
        if ((fiber == null) || (type == null)) return null;
        try { convertAndSetTime(getSimTime()); } catch (SigscanException e) {
            fiber.setTimeError(new SigscanException(
                e.getMessage()+
                "\n\tbeginTran lab="+label+" desc= "+description),
                getSimTime());
        }
        long ptr = 0;
        long fiber_ptr= fiber.getHandle();
        long type_ptr = type.getHandle();
        if (label == null) label = new String();
        if (description == null) description = new String();
        synchronized(APIlock) {
            if (open)
                ptr = beginTransaction(fiber_ptr, type_ptr, label, description);
        }
        return new Transaction(fiber, ptr);
    }   

    Transaction beginTransaction(
            TransactionFiber fiber, TransactionType type,
            String label, String description, long time) {
        //if (time >= getSimTime()) {
            Transaction tran = new Transaction(fiber);
            BeginTranEvent bte = new BeginTranEvent(this, fiber, time,
                                                type,tran, 
                                                label, description);
            addEvent(bte);
            return tran;
        //}
        //System.err.println("BeginTran "+label+" scheduled for "+time+
        //                   " is in the past (currTime = "+getSimTime()+
        //                   "), beginTransaction ignored");
        //return null;
        /*
        //Set the time
        try { convertAndSetTime(time); } catch (SigscanException e) {
            fiber.setTimeError(new SigscanException(
                e.getMessage()+
                "\n\tbeginTran lab="+label+" desc= "+description));
        }
        return beginTransaction(fiber, type, label, description);
        */
    }

    private static native long getSuccessorHandle(long sdiHandle);

    private static native long getPredecessorHandle(long sdiHandle);

    /** The actual linking call **/
    private static native void linkTransactions(long linkHandle,
                                                  long src_handle,
                                                  long dst_handle);

    /**Links the pred Transaction as the predecessor to the Transaction src.
     * @param src The base transaction
     * @param pred The predecessor
     **/
    void linkPredecessor(Transaction src, Transaction pred) {
        if ((src == null) || (pred == null)) return;
        long src_ptr = src.getHandle();
        long pred_ptr = pred.getHandle();
        if ((src_ptr == 0) || (pred_ptr==0)) return;
        synchronized(APIlock) {
            if (open) linkTransactions(plink, src_ptr, pred_ptr); 
        }
    }
   
    /**Links the pred Transaction as the predecessor to the Transaction src.
     * @param src The base transaction
     * @param pred The predecessor
     * @param time The time at which both transactions are valid
     **/
    public void linkPredecessor(Transaction src, Transaction pred, long time) {
        if (time > getSimTime()) 
            addEvent(new LinkEvent(this, src, pred, LinkEvent.PRED,time));
        else
            linkPredecessor(src, pred);
    }                    

    /**Links the pred Transaction as the predecessor to the Transaction src.
     * @param src The base transaction
     * @param succ The successor
     * @param time The time at which both transactions are valid
     **/
    public void linkSuccessor(Transaction src, Transaction succ, long time) {
        if (time > getSimTime()) 
            addEvent(new LinkEvent(this, src, succ, LinkEvent.SUCC,time));
        else
            linkSuccessor(src, succ);
    }                    
    
    /**Links the pred and succ transactions in both directions.
     * @param first_tran The transaction that comes first in time
     * @param second_tran The transaction that comes second in time
     * @param time The time at which both transactions are valid
     **/
    public void link(Transaction first_tran, Transaction second_tran,
                              long time) {
        if (time > getSimTime()) 
            addEvent(new LinkEvent(this, first_tran, second_tran,
                                   LinkEvent.BOTH,time));
        else
            link(first_tran, second_tran);
    }                    
    
    /**Links the succ Transaction as the successor to the Transaction src
     * @param src The base transaction
     * @param succ The successor
     **/
    void linkSuccessor(Transaction src, Transaction succ) {
        if ((src == null) || (succ == null)) return;
        long src_ptr = src.getHandle();
        long succ_ptr = succ.getHandle();
        if ((src_ptr == 0) || (succ_ptr==0)) return;
        synchronized(APIlock) {
            if (open) linkTransactions(slink, src_ptr,succ_ptr);
        }
    }

    /** Links the transactions in both directions, assumes that first_tran
     * comes first in time
     * @param first_tran The first transaction chronologically
     * @param second_tran The second transaction chronologically
     * **/
    public void link(Transaction first_tran, Transaction second_tran) {
        linkPredecessor(second_tran, first_tran);
        linkSuccessor(first_tran, second_tran);
    }
    
    private static native void endLastTransaction(long fiberHandle);
   
    /** Ends the last opened transaction on the fiber
     * @param fiber The fiber of the transaction **/
    void endLastTransaction(TransactionFiber fiber) {
        if (fiber == null) return;
        try { convertAndSetTime(getSimTime()); } 
        catch (SigscanException e) { fiber.setTimeError(e,getSimTime()); }
        long fiber_ptr = fiber.getHandle();
        synchronized(APIlock) { if (open) endLastTransaction(fiber_ptr); }
    }
    
    /** Ends the last opened transaction on the fiber at the time given
     * If the time is in the future, this logging action is scheduled
     * and done later
     * @param fiber The fiber of the transaction\
     * @param time The time for this event to take place**/
    public void endLastTransaction(TransactionFiber fiber, long time) {
        //if (time >= getSimTime()) {
            EndTranEvent ete = new EndTranEvent(this, fiber, time);
            addEvent(ete);
        //} else {
        //    System.err.println("Attempting to end last transaction "+
        //                       "in the past, "
        //                       +" end Time= "+time+" , currTime = "+
        //                       getSimTime());
            /*
            //Set the time
            try { convertAndSetTime(time); } 
            catch (SigscanException e) { fiber.setTimeError(e,time); }
            endLastTransaction(fiber);
            */
        //}
    }
        
    private static native void endTransaction(long fiberHandle,
                                                long tranHandle);

    /** Ends the transaction specified on the given fiber.
     * Note that the endLastTransaction() function is probably
     * a better method for ending transactions, because it won't
     * disrupt the nesting of transactions.
     * @param fiber The fiber the transaction is on
     * @param tran The transaction to end
     * @see com.avlsi.tools.sigscan.TransactionFiber
     * */
    void endTransaction(TransactionFiber fiber, Transaction tran) {
        if ((fiber == null)  || (tran==null))return;
        try { convertAndSetTime(getSimTime()); } 
        catch (SigscanException e) { fiber.setTimeError(e,getSimTime()); }
        long fiber_ptr = fiber.getHandle();
        long tran_ptr = tran.getHandle();
        synchronized(APIlock) {
            if (open) endTransaction(fiber_ptr, tran_ptr);
        }
    }
   
    /** Ends the transaction specified on the given fiber at the
     * given time.  If the time is in the future, then the logging
     * action will be scheduled and done later.
     * Note that the endLastTransaction() function is probably
     * a better method for ending transactions, because it won't
     * disrupt the nesting of transactions.
     * @param fiber The fiber the transaction is on
     * @param tran The transaction to end
     * @param time The time that this action happens
     * @see com.avlsi.tools.sigscan.TransactionFiber
     * */
    public void endTransaction(TransactionFiber fiber, Transaction tran,
                               long time) {
        //if (time > getSimTime()) {
            EndTranEvent ete = new EndTranEvent(this, fiber, tran, time);
            addEvent(ete);
        //} else { 
        //    System.err.println("Attempting to transaction "+
        //                       "in the past, "
        //                       +" end Time= "+time+" , currTime = "+
        //                       getSimTime());
            /*//Set the time
            try { convertAndSetTime(time); } 
            catch (SigscanException e) { fiber.setTimeError(e,time); }
            endTransaction(fiber, tran);
            */
        //}
    }
        
    //
    //Attribute calls
    //
    
    private static native long attachAttribute(int type, long tranType,
                                           String name);
    
    /**Attaches the Attribute to the given type.
     * Shouldn't be called directly
     * @param attr The Attribute to attach
     * @param type The target TransactionType to attch to.
     * @return A native address to the bound attribute
     * @see com.avlsi.tools.sigscan.Attribute
     **/
    public NativeHandle attachAttribute(Attribute attr,
                                  TransactionType type) {
        long ptr = 0;
        long type_ptr = type.getHandle();
        synchronized(APIlock) {
            if (open)
                ptr = attachAttribute(attr.getType(),type_ptr, attr.getName());
        }
        return new NativeHandle(ptr);
    }
  
    /** Schedules an event to set the attribute
     * @param attr The attribute to set
     * @param value The AttributeValue to set it to
     * @param time The time to set it at
     * @see com.avlsi.tools.sigscan.Attribute
     **/
    public void setAttribute(Attribute attr, AttributeValue value, long time) {
        //System.out.println(time+": Scheduling Attribute "+attr+" = "+value+
        //        " on fiber "+attr.getTranType().getFiber().getFullname());
        //if (time >= getSimTime()) {
            AttributeEvent ae = new AttributeEvent(attr, value, time);
            addEvent(ae);
        //} else
        //    attr.set(value);
    }
    
    /**
     * Sets an IntAttribute to <code>value</code>.
     * Probably shouldn't be called directly
     * @param attr The target attribute
     * @param value the new value
     * @see com.avlsi.tools.sigscan.IntAttribute
     * */
    void setIntAttribute(IntAttribute attr, int value) {
        synchronized(APIlock) { 
            if (open) setIntAttribute(attr.getHandle(), value); }
    }
    
    private static native void setIntAttribute(long attrHandle,
                                                 int value);                                          
    /**
     * Sets an StringAttribute to <code>value</code>.
     * Probably shouldn't be called directly
     * @param attr The target attribute
     * @param value the new value
     * @see com.avlsi.tools.sigscan.StringAttribute
     * */
    void setStringAttribute(StringAttribute attr, String value) {
        synchronized(APIlock) { 
            if (open)setStringAttribute(attr.getHandle(), value); }
    }
    
    private static native void setStringAttribute(long attrHandle,
                                                 String value);                                          
    /**
     * Sets an LongAttribute to <code>value</code>.
     * Probably shouldn't be called directly
     * @param attr The target attribute
     * @param value the new value
     * @see com.avlsi.tools.sigscan.LongAttribute
     * */
    void setLongAttribute(LongAttribute attr, long value) {
        synchronized(APIlock) { 
            if (open) setLongAttribute(attr.getHandle(), value); }
    }
    
    private static native void setLongAttribute(long attrHandle,
                                                 long value);                                          
    /** @return The timeScale of the database.  For instance, if this call 
     * returns -9, then the smallest unit of time is 1 nanosecond.
     ***/
    public int getTimeScale() { return timeScale;}
   
    /** @return The most recently instantiated Sigscan object **/
    public static Sigscan getMostRecentSigscan() { return mostRecent; }

    //
    //Constructors
    //
    
    /**
     * Constructor.
     * @param filename Filename for the database.
     * @param useAlreadyOpenDatabase If database is already open, use it
     * @param timeScale 10 ^ timeScale is equal to the smallest unit of time
     * in the database.  All time calls should be in these units.
     * @exception SigscanException If the libray can't be loaded, or if the 
     * database file can't be created
     **/
    public Sigscan(String filename,
                   boolean useAlreadyOpenDatabase, int timeScale)
            throws SigscanException {
        loadLib();
        synchronized(APIlock) {
            sdiHandle=
                openDatabase(filename,useAlreadyOpenDatabase, timeScale);
            slink = getSuccessorHandle(sdiHandle);
            plink = getPredecessorHandle(sdiHandle);
        }
        this.timeScale = timeScale;
        open = true;
        mostRecent = this;

        cleanupThread =
            new Thread(
                new Runnable() {
                    public void run() { close(); }
                }
            );

        Runtime.getRuntime().addShutdownHook(cleanupThread);
        this.name = filename;

    }
    
    /**
     * Constructor, same as above except uses the default value of 
     * useAlreadyOpenDatabase = false
     * @param filename The name of the database
     * @param timeScale 10 ^ timeScale is equal to the smallest unit of time
     * in the database.  All time calls should be in these units.
     * @throws SigscanException If the libray can't be loaded, or if the 
     * database file can't be created
     **/
    public Sigscan(String filename, int timeScale)
            throws SigscanException {
        this(filename, false, timeScale);
    }

    /**
     * Constructor, same as above except uses the default values of 
     * useAlreadyOpenDatabase = false and timeScale=-9
     * @param filename The name of the database
     * @throws SigscanException If the libray can't be loaded, or if the 
     * database file can't be created
     **/
    public Sigscan(String filename)
            throws SigscanException {
        this(filename, false, -9);
    }

    public String getName() { return name; }
}

