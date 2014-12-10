/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan;

//import com.avlsi.tools.dsim.DigitalScheduler;

import com.avlsi.util.debug.Debug;

import java.util.ArrayList;

/**
 * Class for a new entry in the signalscan database.  Each object
 * that will have transactions will need a fiber.  Also handles scheduling
 * for non-universal timed simulators (like TSim).
 *
 * @author Dan Daly
 * @version $Date$
 **/
public class TransactionFiber extends NativeHandle{

    /*Scope of this fiber*/
    private final String scopename;
    /*Name of this fiber */
    private final String fibername;
    /*Database holding this fiber */
    private final Sigscan sigscan;
   
    /**The current open transaction **/
    private ArrayList trans =new ArrayList();
   
    //private String currType=null;
    private ArrayList types = new ArrayList();
    
    /**
     * Constructor.
     * @param scopename The scope to place this fiber in
     * @param fibername The name of this fiber
     * @param sigscan The database to place this fiber in
     **/
    public TransactionFiber(String scopename, String fibername,
                            Sigscan sigscan) {
        super(sigscan != null?
              sigscan.PRIVATEnewTransactionFiber(scopename, fibername):
              0);
        this.sigscan = sigscan;
        this.scopename = scopename;
        this.fibername = fibername;
    }

    /**Builds a new TransactionType
     * @param trankind The kind of type, Sigscan.BEGINEND, etc.
     * @param typename Name of this TransactionType.  Note that if a 
     * TransactionType of the same name and kind already exists,
     * that TransactionType wil be returned.
     */
    public TransactionType newTransactionType(String typename, int trankind) {
        return new TransactionType(typename, trankind, this); }
    
    /** Builds a new BEGINEND type.
     * @param typename Name of this TransactionType.  Note that if a 
     * TransactionType of the same name and kind already exists,
     * that TransactionType wil be returned.
     * @return the new BEGINEND TransactioType
     * */
    public TransactionType newBeginEndType(String typename) {
        return new TransactionType(typename, 
                                   Sigscan.KIND_BEGINEND,this); }

    /** Builds a new EVENT type.
     * @param typename Name of this TransactionType.  Note that if an
     * TransactionType of the same name and kind already exists,
     * that TransactionType wil be returned.
     * @return the new EVENT TransactioType
     * */
    public TransactionType newEventType(String typename) {
        return new TransactionType(typename, 
                                   Sigscan.KIND_EVENT,this); }
    
    /** Builds a new ERROR type.
     * @param typename Name of this TransactionType.  Note that if an
     * ERROR TransactionType of the same name and kind already exists,
     * that TransactionType wil be returned.
     * @return the new ERROR TransactioType
     * */
    public TransactionType newErrorType(String typename) {
        return new TransactionType(typename,
                                   Sigscan.KIND_ERROR,this);
    }
   
    /**Starts a new transaction.
     * @param type The type of the transaction.  If the type is BEGINEND, then 
     * the transaction must be ended to be valid.
     * @param label transaction label
     * @param desc description
     * @return A Transaction object which can be used to link this transaction
     * with other transactions
     **/
    public Transaction beginTransaction(TransactionType type,
                                 String label,
                                 String description) {
        //Debug.assertTrue(false);
        //System.err.println("Watch out, untimed beginTran");
        if (sigscan != null) {
            Transaction tran = sigscan.beginTransaction(this, type,
                                            label, description);
            //this.currTran = tran;
            return tran;
        }
        return null;
    }

    private static int BEGINEND = Sigscan.KIND_BEGINEND;

    /**Starts a new transaction.
     * @param type The type of the transaction.  If the type is BEGINEND, then 
     * the transaction must be ended to be valid.
     * @param label transaction label
     * @param desc description
     * @param time The time to do this action at
     * @return A Transaction object which can be used to link this transaction
     * with other transactions
     **/
    public Transaction beginTransaction(TransactionType type,
                                 String label,
                                 String description,
                                 long time) {
        //if (type.getKind() == BEGINEND)
            //System.out.println(time+" : beginning "+type+
            //                       " on fiber "+getFullname());
        if (sigscan != null) {
            if ((!trans.isEmpty()) && types.contains(type.toString())) {
                System.err.println(time+" : ERROR transaction of type "+
                                   type.toString()+" already open "+
                                   "on fiber "+getFullname());
            }
            Transaction tran = sigscan.beginTransaction(this, type,
                                            label, description,time);
            if (type.getKind() == BEGINEND) {
                trans.add(tran);
                types.add(type.toString());
            }
            return tran;
        }
        return null;
    }

    /**Ends the most recently opened transaction.**/
    public void endTransaction() {
        Debug.assertTrue(false);
        if (sigscan != null) {
            sigscan.endLastTransaction(this);
            //currTran = null;
        }
    }
    
    /**Ends the most recently opened transaction.
     * @param time The time to do this action at
     * **/
    public void endTransaction(long time) {
        if (sigscan != null) {
            if (trans.isEmpty()) {
                System.err.println(time+" : ERROR no transaction open "+
                                   "on fiber "+getFullname());
            } else {
                //System.out.println(time+" : ending "+
                //                   ((String) types.get(types.size()-1))+
                //               " on fiber "+getFullname());
                trans.remove(0);
                types.remove(0);
            }
            sigscan.endLastTransaction(this,time);
        }
    }
    
    /**Ends the Transaction tran.  This method is not encouraged- if
     * the transactions are not nested correctly in the database, the
     * log will give unpredictable results.  So use endTransaction() or
     * use with caution.
     * @param tran The Transaction to be ended
     **/
    public void endTransaction(Transaction tran) {
        Debug.assertTrue(false);
        if ((sigscan != null) && (tran != null)){
            sigscan.endTransaction(this,tran);
            //if (tran == currTran) currTran = null;
        }
    }

    /**Ends the Transaction tran.  This method is not encouraged- if
     * the transactions are not nested correctly in the database, the
     * log will give unpredictable results.  So use endTransaction() or
     * use with caution.
     * @param tran The Transaction to be ended
     * @param time The time to do this action at
     **/
    public void endTransaction(Transaction tran,long time) {
        if ((sigscan != null) && (tran != null)){
            
            sigscan.endTransaction(this,tran,time);
            //if (tran == currTran) currTran = null;
        }
    }
    //
    //JavaChannel Scheduling
    //

    /**Schedule a new BEGINEND event onto the digitalScheduler.  Note that this
     * command blocks until the event is logged....
     * @param ev A BeginTranEvent
     * @return The transaction when the event is fired.**/
    public Transaction scheduleBeginTranEvent(BeginTranEvent ev) {
        if (sigscan != null) {
            scheduleEvent(ev);
            return ev.getTransaction();
        } return null;
    }
            
    /**Schedule a new event onto the digitalScheduler.  Note that this
     * command blocks until the event is logged....
     * @param ev A LogEvent
     * **/
    public synchronized void scheduleEvent(LogEvent ev) {
        //System.out.println("Scheduling event @ "+ev.getTime());
        //ev.setNotifyFlag(true);
        //DigitalScheduler.get().addEvent(ev);
        ev.add();
        /*
        try {
            System.err.println("Waiting.");
            DigitalScheduler.get().decThreadCount();
            wait();
        } catch (InterruptedException e) {
            System.out.println("Interrupted.");
        }*/
        //System.out.println("Scheduling Done.");
    }

    /**Wake up the thread and inc the thread count **/
    public synchronized void doNotify() {
        //System.err.println("Notifying.");
        //notify();
        //DigitalScheduler.get().incThreadCount();
    }
       
    private TransactionType timeErrorTT = null;
    private StringAttribute stackAttr = null;
    private Transaction lastError = null;
    /*Called when a timing error is found.*/
    public void setTimeError(SigscanException e,long time) {
        if (sigscan != null) {
            if (timeErrorTT == null) {
                timeErrorTT = newErrorType("timeError");
                stackAttr = new StringAttribute("stack trace",
                                                timeErrorTT,
                                                new DebugOpts().logAllSST());
            }
            Transaction tran = beginTransaction(
                    timeErrorTT, "time error @ "+time, e.getMessage());
            java.io.StringWriter sw = new java.io.StringWriter();
            java.io.PrintWriter pw = new java.io.PrintWriter(sw);
            e.printStackTrace(pw);
            stackAttr.set(sw.toString());
            sigscan.reportError(tran);
        }
    }

    /** @return The sigscan holding this fiber **/
    public Sigscan getSigscan() { return sigscan; }

    /** @return The current open transaction **/
    //public Transaction getCurrentTransaction() { return currTran; }

    public String getFullname() { return scopename+"."+fibername; }

}

