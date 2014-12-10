/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.util.Iterator;
import java.util.ArrayList;
//Use hashtable because its synchronized, as opposed to HashMap
import java.util.Hashtable;
import java.util.Iterator;

import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.DebugOpts;
import com.avlsi.tools.sigscan.SigscanException;
import com.avlsi.tools.sigscan.ChannelSigscan;

import com.avlsi.tools.dsim.SequencedEvent;
import com.avlsi.tools.dsim.DigitalScheduler;
import java.math.BigInteger;

import com.avlsi.tools.dsim.Node;

/**
 * <p> Class for an event-driven bus. The bus can be shared between bus drivers,
 * but in a multiple driver situation only one bus can drive at a time.  All
 * others must drive to high Z.  Any number of devices can <code>get()</code> or
 * <code>wait()</code> on the bus. </p>
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class SharedBus {

    //public static double convFactor = 1e-9d/1800d;
    public static double convFactor = 1800d/1e-9d;
    public static final double sec =    1d;
    public static final double msec =   1e-3d;
    public static final double usec =   1e-6d;
    public static final double nsec =   1e-9d;
    public static final double psec =   1e-12d;
    public static final double fsec =   1e-15d;
    //
    //Wait Parameters
    /** Tells the wait method to wait for a postiive edge
     * on any of the pins on the bus**/
    public static final int POSEDGE = 1;
    /*  Tells the wait method to wait for a negative edge on
     *  any of the pins on the bus*/
    public static final int NEGEDGE = -1;
    /**Wait for any pin to change on the bus**/
    public static final int ANYEDGE = 0;
    
    /**busSize or width of the bus**/
    protected final int busSize;

    /** SST logging handles for each pin**/
    private long[] handles=null;

    /**The scopename of this bus*/
    private final String scopename;
    /*The name*/
    private final String name;
    /*Its logging database pointer*/
    public final Sigscan sigscan;

    /*The debugging options for this bus */
    private final DebugOpts opts;

    /*The current Data chunk*/
    //private Data data;
    //The current value of the bus
    //private final int[] aval;
    //private final int[] bval;
    //private final int[] pendingAval;
    //private final int[] pendingBval;
    protected final Data data;
    //protected final Data pending;

    protected SharedBusEvent ev;
    
    /**A table of all the drivers driving the bus*/
    private final Hashtable drivers;

    //
    //Wait objects and wait counts
    //
    private int wCount=0, pCount=0, nCount=0;
    private final Object wLock = new Object();
    private final Object poslock = new Object();
    private final Object neglock = new Object();

    //Used for forcing pin values
    //
    protected final BusDriver forceDriver = new ForceDriver();

    //
    //List of Watchers
    //
    private final ArrayList watches=new ArrayList();

    /**
     * Building a SharedBus
     * @param scopename the scope or location of the bus in the design
     * @param name the name of the bus
     * @param busSize the size of the bus
     * @param sigscan The database file for automatic logging, can be
     * set to null to diable logging
     **/
    public SharedBus(String scopename, String name,
                     int busSize, Sigscan sigscan) {
        this(scopename, name, sigscan, new DebugOpts(), busSize);
    }
    
    public SharedBus(String scopename, String name,
                     Sigscan sigscan, DebugOpts opts,
                     int busSize) {

        this.busSize = busSize;
        assert(busSize > 0);
        //this.data = new Data(Data.pad(busSize, 'Z'));
        this.data = new Data(busSize);
        //this.pending = new Data(busSize);
        ev = new SharedBusEvent(this);
        data.setAllTo('z');
        this.scopename = scopename;
        this.sigscan = sigscan;
        this.opts = opts;
        this.name = name;
        drivers = new Hashtable();

        //Signalscan Logging
        handles = null;
        isLogging(); //Note has side effects!
    }

    protected boolean isLogging() {
        if (sigscan == null) return false;
        if (!opts.loggingBuses()) return false;
        if (handles == null) startLogging();
        return true;
    }

    protected void startLogging() {
        handles = new long[busSize];
        for(int loop=0;loop<busSize;loop++) {
            handles[loop] = 
                sigscan.newLogicVariable(getScopename(),
                        (busSize == 1)?getName():getName()+loop);
            //Assumes that the DigitalScheduler has taken care
            //of the current time when initializing
            //TODO:  Make sigscan take ava;/bval numbers
            sigscan.logicChange(handles[loop],
                                getValueAt(loop));
        }
    }

    /**Loops through all the drivers, poling its value at pin 
     * <code>index</code>.  Calculates the final driven value
     * and returns it.
     **/
    /*private char recalculate(int index) {
        char currValue = 'Z';
        //System.err.println(getFullname()+" has "+drivers.size()+" drivers.");
        for(Iterator i=drivers.values().iterator();i.hasNext();) {
            Data temp = (Data) i.next();
            int val = temp.getValueAt(index);
            //If driven Z, ignore
            if (val == Data.LOGICZ) continue;
            //If any are driven X, the value is X
            if (val == Data.LOGICX) return 'X';
            //if not...
            if (currValue == 'Z') currValue = temp.getCharAt(index);
            //if multiple driving with data, return X
            else return 'X';
        }
        return currValue;
    } */      
        
    /*private final void setToPendingValues(int[] newaval, int[] newbval) {
        for (int loop=0;loop<aval.length;loop++) {
            if (aval[loop] != newaval[loop]) {
                valChange = true;
                if (aval[loop] < newaval[loop]) rose = true;
                else fell = true;
            }
            aval[loop] = newaval[loop];
            if (bval[loop] != newbval[loop]) valChange = true;
            bval[loop] = newbval[loop];
        }
    }*/
    
    /*Bus Event Handler
     **/
    public void recordChange(BusDriver drv, Data newdata, long time) { 
        boolean valChange=false;
        boolean rose = false, fell = false;

        //First, record the pin change
        //System.err.println(drv+" Driving "+getFullname()+" to "+newdata+" at "+time);
        if (drv.equals(forceDriver)) {
                //If we are forcing this pin, then remove any other setters.
                drivers.clear();
        }
        if (!drivers.containsKey(drv))
            drivers.put(drv,newdata.clone());
        int ret = 0;
        if (drivers.size() == 1) {
            ret = data.setTo(newdata);
            
            //System.out.println("Changed= "+valChange+" rose= "+rose+" fell= "+ fell);
        } else {
            Data combNewdata = 
                Data.calculateCombinedValue(drivers.values().iterator());
            ret = data.setTo(combNewdata);
            //for(Iterator i=drivers.iterator();i.hasNext();) {
            //    for (int loop=0;loop<
        }
        valChange = ret != 0;
        rose = ret == 1;
        fell = ret == -1;
        //if (valChange) {
        
            if (isLogging()) {
                try {
                    sigscan.convertAndSetTime(time);
                } catch (SigscanException e) {
                    System.err.println("SharedBus: "+getFullname()+" : "+e.getMessage());
                    sigscan.incErrorCount();
                }
                for (int loop=0;loop<busSize;loop++) {
                    sigscan.logicChange(handles[loop],getValueAt(loop));
                }
            }
            reportToWatchers(time,rose, fell);
            notifyAll(rose, fell);
        //}
    }

    /**@return The width or size of this bus **/
    public int getBusSize() { return busSize; }
    
    /**$return The name of this bus **/
    public String getName() { return name; }

    /**@return The name of the scope of this bus**/
    public String getScopename() { return scopename; }

    /**@return The name of the scope of this bus**/
    public String getScope() { return getScopename(); }

    /**@return The fullname of this bus**/
    public String getFullname() { return scopename+"."+name; }

    /**@return The DebugOpts object for this bus **/
    public DebugOpts getDebugOpts() { return opts; }

    /**
     * @return the data driven on the bus at the current time in the
     * digital scheduler
     **/
    public Data get() { return data; }

    /**
     * Returns a section of the bus, remember that the least sig digit
     * is at index zero.
     * @param startIndex The starting index
     * @param endIndex The ending index
     * @return string of pins [endIndex...startIndex]
     **/
    public String getStringSection(int startIndex, int endIndex) {
        return data.getStringSection(startIndex, endIndex);
    }
    
    /**
     * @return A String representing the value on the bus [msb .. lsb]
     **/
    public String getStringData() { return data.getStringData(); }

    /**@return A long representation of the values on the bus.  Note
     * the char to bit conversion:
     * <pre>
     * '1' - 1
     * '0' - 0
     * 'Z' - 0
     * 'X' - 1
     * </pre>
     **/
    public boolean getBooleanData() { return data.getBooleanData(); }

    public int getIntData() { return data.getIntData(); }
        
    public long getLongData() { return data.getLongData(); }
    
    public int getValueAt(int index) { return data.getValueAt(index); }

    /**@return A BigInteger representation of the values on the bus.  Note
     * the char to bit conversion:
     * <pre>
     * '1' - 1
     * '0' - 0
     * 'Z' - 0
     * 'X' - 1
     **/
    public BigInteger getBigIntegerData() { 
        return data.getBigIntegerData();
    }

    /** @return true if all pins are high Z **/
    public boolean isZ() {
        return data.isZ();
    }
            
    /** @return true if all pins are X **/
    public boolean isX() {
        return data.isX();
    }
 
    public void force(Data pending, long delay) {
        set(forceDriver, pending, delay);
    }

    /** The main set method. This method, given a driver, will set the
     * bus to the pending data with the given delay.
     * Multiple drivers are treated in the same way as in verilog-
     * that is that no two drivers can drive a numeric signal at the same
     * time.  The high Z or open collector value is used to differentiate
     * between driving and not driving a signal.  For example, if:<BR>
     * <pre>
     *
     *   Driver 1:  Drove ZZZZ1ZZZ at 15
     *   Driver 2:  Drove 0ZZZZZZX at 150
     * </pre>
     * <p>
     * Then the resulting pin would be set to ZZZZ1ZZZ at 15, and
     * then set to 0ZZZ1ZZZX at time 150.
     **/
    public void set(BusDriver driver, Data pending, long delay) {
        //if (!(driver instanceof NativeBusDriver)) {
            //System.out.println("Setting "+getFullname()+" to "+pending+" @"+
            //        (DigitalScheduler.get().getTime()+delay)+" delay= "+delay);
        //}
        //
        if (delay < 0) {
            new Exception("Neg Delay Set").printStackTrace();
        }
        /*if (index >= 0) { //already queued
            if (event_time != (DigitalScheduler.get().getTime()+delay)) {
            System.err.println(getTime()+" Bus "+getFullname()+" has already been "+
                               "queued to transistion.  event_time= "+event_time+" pending= "+pending+" quitting.");
            System.exit(15);
            }
        }*/
        
        //this.driver = driver;
        //this.event_time = DigitalScheduler.get().getTime()+delay;
        if (ev.getIndex() >= 0) { //already queued
            //System.err.println(getFullname()+" already has event queued...");
            //System.exit(1);
            //Build a new one
            DigitalScheduler.get().addEvent(new SharedBusEvent(this,
                driver, pending, DigitalScheduler.get().getTime()+delay));
        } else {
            assert(!ev.hasNextEvent());
            ev.set(driver, pending, DigitalScheduler.get().getTime()+delay);
            DigitalScheduler.get().addEvent(ev);
        }
    }
/*
    public void set(BusDriver driver, Data newval, long delay) {
        System.out.println("Setting "+getFullname()+" to "+newval+" @ "+
                (DigitalScheduler.get().getTime()+delay));
        pending.setTo(newval);
        set(driver, pending, delay);
    }
*/
    public void force(int[] newaval,int[] newbval,long delay) {
        set(forceDriver, newaval, newbval, delay);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The Data to drive to
     * @param delay delay from current time to trigger the change
     **/
    public void set(BusDriver driver,int[] newaval,int[] newbval,long delay) {
        Data pending = new Data(busSize);
        pending.set(newaval, newbval);
        set(driver,pending,  delay);
    }

    public static long convert(double time, double units) {
        return Math.round(convFactor*time*units);
    }

    public static double getSecs(long dsimTime) {
        return dsimTime/convFactor;
    }      
    
    public void force(int newval, 
                    double timeDelay, double units) {
        set(forceDriver, newval, timeDelay, units);
    }

    /**Sets the bus to newval with a delay in real time (equal to timeDelay*units seconds)
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The int value to drive it to 
     * @param timeDelay  The time delay of this event
     * @param units the units on this delay ( sec, msec, usec, nsec, psec, etc)
     **/
    public void set(BusDriver driver, int newval, 
                    double timeDelay, double units) {
        set(driver, newval, convert(timeDelay,units));
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param val The int value to drive the bus to
     * @param delay delay from current time to trigger the change
     **/
    public void set(BusDriver driver, int newval, long delay) {
        Data pending = new Data(busSize);
        pending.set(newval);
        set(driver, pending, delay);
    }
    
    public void force(long[] newval, long delay) {
        set(forceDriver, newval, delay);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The long values to drive it to
     * @param delay delay from current time to trigger the change
     **/
    public void set(BusDriver driver, long[] newval, long delay) {
        
        Data pending = new Data(busSize);
        pending.set(newval);
        set(driver, pending, delay);
    }
    
    public void force(long[] newval, double timeDelay, double units) {
        set(forceDriver, newval, timeDelay, units);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The long array values to drive it to
     * @param timeDelay delay from current time to trigger the change
     * @param units units of the time delay( sec, etc)
     **/
    public void set(BusDriver driver, long[] newval, double timeDelay, double units) {
        set(driver, newval, convert(timeDelay,units));
    }
    
    public void force(long newval, long delay) {
        set(forceDriver, newval, delay);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The long value to drive it to
     * @param delay delay from current time to trigger the change
     **/
    public void set(BusDriver driver, long newval, long delay) {
        
        Data pending = new Data(busSize);
        pending.set(newval);
        set(driver, pending, delay);
    }
    
    public void force(long newval, double timeDelay, double units) {
        set(forceDriver, newval, timeDelay, units);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The long array value to drive it to
     * @param timeDelay delay from current time to trigger the change
     * @param units units of the time delay( sec, etc)
     **/
    public void set(BusDriver driver, long newval, double timeDelay, double units) {
        set(driver, newval, convert(timeDelay,units));
    }
    
    /*private static void setBit(int[] arr, int index) {
        arr[ (index-1) / 32 + 1] |= 1<<(index%32);
    }

    private static void clearBit(int[] arr, int index) {
        arr[ (index-1) / 32 + 1] &= ((1<<(index%32))-1) ^ 1<<(index%32);
    }

    private static boolean testBit(int[] arr, int index) {
        return (arr[ (index-1) / 32 + 1] & (1<<(index%32))) >0;
    }

    private static void fill(int[] arr, int num) {
        for (int loop=0;loop<arr.length;loop++) arr[loop] = num;
    }*/

    public void force(BigInteger newval, long delay) {
        set(forceDriver, newval, delay);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The BigInteger value to drive it to (note longs that
     * are too long for the bus will be truncated)
     * @param delay delay from current time to trigger the change
     **/
    public void set(BusDriver driver, BigInteger newval, long delay) {
        Data pending = new Data(busSize);
        pending.set(newval);
        set(driver, pending, delay);
    }
    
    public void force(BigInteger newval, 
                    double timeDelay, double units) {
        set(forceDriver, newval, timeDelay, units);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The BigInteger value to drive it to (note longs that
     * are too long for the bus will be truncated)
     * @param timeDelay delay from current time to trigger the change
     * @param units units of the time delay( sec, etc)
     **/
    public void set(BusDriver driver, BigInteger newval, 
                    double timeDelay, double units) {
        set(driver, newval, convert(timeDelay,units));
    }
    
    private void setAllTo(BusDriver driver, char value, long delay) {
        Data pending = new Data(busSize);
        pending.setAllTo(value);
        set(driver, pending, delay);
    }

    /**Drives the bus high Z on all pins*/
    public void setZ(BusDriver driver) { this.setAllTo(driver,'Z', 0); }

    /*Drives the bus X on all pins*/
    public void setX(BusDriver driver) { this.setAllTo(driver,'X', 0); }

    /**Drives the bus high Z on all pins*/
    public void setZ(BusDriver driver, long delay) { this.setAllTo(driver,'Z', delay); }

    /**Drives the bus high Z on all pins*/
    public void setZ(BusDriver driver, double timeDelay, double units) { 
        this.setAllTo(driver,'Z', convert(timeDelay, units)); }
    
    /*Drives the bus X on all pins*/
    public void setX(BusDriver driver,long delay) { this.setAllTo(driver,'X', delay); }

    /*Drives the bus X on all pins*/
    public void setX(BusDriver driver, double timeDelay, double units) { 
        this.setAllTo(driver,'X', convert(timeDelay, units)); }
    
    /**Drives the bus 0 on all pins*/
    public void set0(BusDriver driver, long delay) { this.setAllTo(driver,'0', delay); }

    /**Drives the bus 0 on all pins*/
    public void set0(BusDriver driver, double timeDelay, double units) { 
        this.setAllTo(driver,'0', convert(timeDelay, units)); }
    
    /**Drives the bus 1 on all pins*/
    public void set1(BusDriver driver, long delay) { this.setAllTo(driver,'1', delay); }

    /**Drives the bus 1 on all pins*/
    public void set1(BusDriver driver, double timeDelay, double units) { 
        this.setAllTo(driver,'1', convert(timeDelay, units)); }
    
    public void force(String newval, long delay) {
        set(forceDriver, newval, delay);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The string value to drive it to (note strings that
     * are too long for the bus will be truncated)
     * @param delay delay from current time to trigger the change
     **/
    public void set(BusDriver driver, String newval, long delay) {
        Data pending = new Data(busSize);
        pending.set(newval);
        set(driver, pending, delay);
    }
    
    public void force(String newval, double timeDelay, double units) {
        set(forceDriver, newval, timeDelay, units);
    }
    
    /**Sets the bus to newval.
     * @param driver The driver of the bus.  Any object implementing
     * the dummy interface BusDriver can drive the bus
     * @param newval The string value to drive it to (note strings that
     * are too long for the bus will be truncated)
     * @param timeDelay delay from current time to trigger the change in real time
     * @param units units of timeDelay (eg sec, etc)
     **/
    public void set(BusDriver driver, String newval, double timeDelay, double units) {
        set(driver, newval, convert(timeDelay,units));
    }
    /**Detach the Driver from the bus (equivalent to setting the
     * driven value to high Z)
     * @param driver The driver to remove from the SharedBus
     **/
    public void detachBusDriver(BusDriver driver) { 
        assert(false);
        //drivers.remove(driver);
    }

    /**
     * Wait method that will unblock on any change in the bus.
     *
     * @param type unknown.
     * @exception InterruptedException when this thread is interrupted.
     **/
    public void wait(int type) throws InterruptedException {
        switch(type) {
          case POSEDGE: 
            synchronized(poslock) {
                //System.err.println("Waiting p="+pCount);
                pCount++;
                DigitalScheduler.get().decThreadCount();
                poslock.wait();
            }
            break;
          case NEGEDGE:
            synchronized(neglock) {
                //System.err.println("Waiting n="+nCount);
                nCount++;
                DigitalScheduler.get().decThreadCount();
                neglock.wait();
            }
            break;
          default:
        //System.out.println(getFullname()+" thread "+Thread.currentThread().getName()+" about to wait wCount= "+wCount);
            synchronized(wLock) {
                //System.out.println(Thread.currentThread().getName()+" received monitor. wCount="+wCount);
                wCount++;
                DigitalScheduler.get().decThreadCount();
                while (wCount > 0) {
                    //System.err.println(Thread.currentThread().getName()+" Waiting w="+wCount);
                    wLock.wait();
                    //System.out.println(Thread.currentThread().getName()+" Alive Possibly.");
                }
                //System.err.println(Thread.currentThread().getName()+" Woke Up w="+wCount);
            }
        }
    }
  
    /** Notify all listeners that a bus has changed value.
     * @param rose True if any of the bits on on the bus transistioned to 1
     * @param fell True if any of the bits on the bus transistioned to 0
     **/
    protected void notifyAll(boolean rose, boolean fell) {
        //System.err.println("Notifying w= "+wCount+
        //                   " p= "+pCount+" n= "+nCount);
        //Thought about randomizing this order to catch
        //sequence time assumptions
        //
        //Wake up general edgers
        //System.out.println(getFullname()+" thread "+Thread.currentThread().getName()+" about to notify");
        synchronized(wLock) { 
            if (wCount > 0) { 
                //Notify everyone waiting on a general edge
                DigitalScheduler.get().incThreadCount(wCount);
                wCount = 0;
                wLock.notifyAll();
                //System.out.println("Notified All from "+Thread.currentThread().getName()+" wCount= "+wCount);
            }
        }
        //Do posedgers first
        synchronized(poslock) { 
            if (rose && (pCount > 0))  {
                DigitalScheduler.get().incThreadCount(pCount);
                pCount = 0;
                poslock.notifyAll();
            }
        }
        //Now wake up the negedgers
        synchronized(neglock) {
            if (fell && (nCount > 0)) {
                DigitalScheduler.get().incThreadCount(nCount);
                nCount = 0;
                neglock.notifyAll();
            }
        }
    }
 
    public void reportToWatchers(long time,boolean rose, boolean fell) {
        synchronized(watches) {
            for(int loop=0;loop<watches.size();loop++) {
            //for (Iterator i = watches.iterator();i.hasNext();) {
                //BusWatcher bw = (BusWatcher)i.next();
                BusWatcher bw = (BusWatcher) watches.get(loop);
                bw.busChanged(this, time);
                if (bw instanceof ClockWatcher) {
                    if (rose) ((ClockWatcher)bw).risingEdge(this, time);
                    if (fell) ((ClockWatcher)bw).fallingEdge(this, time);
                }
            }
        }
    }

    /** Remove from our watcher list. **/
    public boolean removeWatch(BusWatcher w) {
        boolean b = false;
        synchronized(watches) {
            b =  watches.remove(w);
        }
        return b;
    }

    /** Remove all watchers associated with this node **/
    public void removeAllWatchers(){
        synchronized(watches) {
            watches.clear();
        }
    }
    
    /** Add to our watcher list, for change notifications. **/
    public boolean addWatch(BusWatcher w) {
        // Only add if the watcher is already registered
        boolean b = false;
        synchronized(watches) {
            b = watches.contains(w) || watches.add(w);
        }
        return b;
    }
    /** Check presence in our watcher list. **/
    public boolean isWatcher(BusWatcher w) {
        boolean b = false;
        synchronized(watches) {
            b = watches.contains(w);
        }
        return b;
    }

    /** Method to connect a SharedBus to a node.  All events on the
     * node will be represented in the SharedBus, and vice versa.
     * @param node The node to connect to
     * @param position The position in the SharedBus to represent this
     * node in.
     **/
    public void driveNode(Node node, int position) {
        assert((position>=0) && (position<busSize));
        BusConnector bc = new BusConnector(node, this, position);
        this.addWatch(bc);
    }
    
    public void drivenByNode(Node node, int position) {
        assert((position>=0) && (position<busSize));
        BusConnector bc = new BusConnector(node, this, position);
        node.addWatch(bc);
    }
    
    /*************************************************************************/

    /** Event Interface Stuff **/

    /*private long event_time=0;

    private BusDriver driver=null;

    private int index=-1;

    private SequencedEvent next=null;

    //
    //SequencedEvent methods
    //
    public boolean hasNextEvent() { return next != null; }

    public SequencedEvent getNextEvent() { return next; }

    public void setNextEvent(final SequencedEvent se) { next = se; }

    public void fire() { 
        recordChange(driver, pending, event_time);
    }

    public void setIndex(int index) { this.index = index; }
    public int getIndex() { return index; }
    public long getTime() { return event_time; }
    public boolean isRandom() { return false; }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append("SharedBus: "+getFullname());
        buf.append(" currVal= "+data.getStringData());
        buf.append("\n\tpending event @ "+event_time+" = "+
                       pending.getStringData());
        //if (hasNextEvent()) buf.append("\n***Next Event ***\n"+getNextEvent().toString());
        return buf.toString();
    }*/

/*
    private class BusEvent implements SequencedEvent {

        private BusDriver driver;
        
        private final int[] aval, bval;

        private final long time;

        private int index=-1;

        private SequencedEvent next=null;

        public BusEvent(BusDriver driver, int[] aval, int[] bval, long time) {
            this.driver = driver;
            this.aval = aval;
            this.bval = bval;
            this.time = time;
        }
        
        public void fire() { recordChange(driver,aval, bval,time); }

        public void setIndex(int index) { this.index = index; }
        public int getIndex() { return index; }
        public long getTime() { return event_time; }
        public boolean isRandom() { return false; }

        //
        //SequencedEvent methods
        //
        public boolean hasNextEvent() { return next != null; }

        public SequencedEvent getNextEvent() { return next; }

        public void setNextEvent(final SequencedEvent se) { next = se; }

        public String toString() {
            //return getFullname()+" driven to "+
            //       +" @ "+time;
            return "TODO!!!";
        }
                        
    }      
*/
} // end of class SharedBus

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * c-file-style:fulcrum-java-style
 * End:
 */
