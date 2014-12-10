/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

import com.avlsi.util.debug.Debug;

import com.avlsi.tools.tsim.Data;
import com.avlsi.tools.tsim.SharedBus;
import com.avlsi.tools.tsim.NativeBusDriver;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.SchedulerRunner;

import java.util.Hashtable;
import java.util.HashMap;
import java.util.ArrayList;

import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.SigscanException;
import com.avlsi.tools.sigscan.DebugOpts;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class VpiInterface implements NativeBusDriver{
  
    private static boolean showWarn = false;
    private static boolean debug = false;
    
    public static void setWarn(boolean w) { showWarn = w; }
    public static void setDebug(boolean d) { debug = d; }

    private static final ArrayList handlers = new ArrayList();

    /* Removing this.  This will make anything using VpiInterface
     * in the manner that PV1 used it to not work!
     **/
    /*
    static {

        SchedulerRunner.get().setEnable(false);
        Runtime.getRuntime().addShutdownHook(
            new Thread(
                new Runnable() {
                    public void run() { 
                        try {
                            db_close(); 
                        } catch (UnsatisfiedLinkError e) {}
                    }
                }
            ));

    }*/
    
    public static native void db_close();

    public static final int JV_INPUT = 0;
    public static final int JV_OUTPUT = 1;
    
    protected static final Hashtable buses = new Hashtable();

    public static final VpiInterface driver = new VpiInterface();

    /**
     * Constructor.
     **/
    private VpiInterface() {}

    public static long queryNextEventTime() {
        long time = DigitalScheduler.get().getNextEventTime();
        long timeInFemtoSecs = 
            Math.round(SharedBus.getSecs(time)/SharedBus.fsec);
        return timeInFemtoSecs;
    }

    public static long evaluateJavaSimulatorUpTo(long timeInFemtoSecs) {
        long time = SharedBus.convert(timeInFemtoSecs, SharedBus.fsec);
        DigitalScheduler dsim = DigitalScheduler.get();
        dsim.cycleTo(time+1); //Include the time given
        return queryNextEventTime();
    }
    
    public static long getTimeInFemtos(long timeInDsim) {
        return Math.round(SharedBus.getSecs(timeInDsim)/SharedBus.fsec);
    }
    public static synchronized 
        void setBus(long handle, Data newval, long delayTime) {
        //System.out.println("Setting "+handle+" to "+newval+
        //                   " @ "+delayTime);
        setBus(handle, newval.getAval(), newval.getBval(),
               getTimeInFemtos(delayTime+getTime()));
    }

    public static synchronized 
        void forceBus(long handle, Data newval, long delayTime) {
        //System.out.println("Setting "+handle+" to "+newval+
        //                   " @ "+delayTime);
        forceBus(handle, newval.getAval(), newval.getBval(),
                 getTimeInFemtos(delayTime+getTime()));
    }

    public static synchronized 
        void releaseBus(long handle, Data val, long delayTime) {
        //System.out.println("Setting "+handle+" to "+newval+
        //                   " @ "+delayTime);
        releaseBus(handle,val.getAval(), val.getBval(),
                   getTimeInFemtos(delayTime+getTime()));
    }

    public static void handleSetBus(String name, long[] value, int length, 
                                    long timeInFemtoSecs) {
        VerilogSharedBus bus = (VerilogSharedBus) buses.get(name);
        long time = 
            SharedBus.convert(timeInFemtoSecs, SharedBus.fsec);
        if ((bus == null) && (time != 0)){
            //Ignore unknown buses at t=0
            System.err.println(time+" : Bus "+name+" set in the Verilog "+
                               "is not registered with VerilogInterface "+
                               "or it does not exist.");
            return;
        }
        if ((time - getTime()) == -1) {
            //This is caused by rounding in the conversion from femto
            //to dsim units.  In this particular case, round up so that
            //the event does not fire in the past.
            //One DSim unit should be negligible...
                bus.set(driver, value[0], 0);
        } else {
            bus.set(driver, value[0], time - getTime()); //computes delay
        }
    }

    public static long getTime() { return DigitalScheduler.get().getTime(); }

    public static void registerBus(SharedBus bus) {
        buses.put(bus.getFullname(), bus);
    }


    private static native void setBus(long handle, int[] aval, int[] bval, long time);
    private static native void forceBus(long handle, int[] aval, int[] bval, long time);
    private static native void releaseBus(long handle, int[] aval, int[] bval,long time);
    private static native long get_handle_by_name(String fullName);
    private static native int get_size_by_handle(long handle);
    private static native void doSignalCallback(
            int index, long handle, int mode);
    private static native boolean getValue(long handle, int[] aval, int[] bval);

    private static ArrayList busArray = new ArrayList();
    
    public static int addBus(VerilogSharedBus bus) {
        synchronized(busArray) { 
            busArray.add(bus); 
            return busArray.size()-1;
        }
    }
    
    private static int findIndex(String scopename, String name) {
        String pinName = scopename+"."+name;
        int foundIndex = -1;
        synchronized(busArray) {
            for (int loop=0;loop<busArray.size();loop++) {
                VerilogSharedBus bus = (VerilogSharedBus) busArray.get(loop);
                if (bus.getFullname().equals(pinName)) {
                        foundIndex = loop;
                        break;
                }
            }
        }
        return foundIndex;
    }

    public static int connectPin(String scopename, String name,
                                 int busSize, long handle, int mode) {
        int foundIndex = findIndex(scopename, name);
        if (foundIndex < 0) {
            if (showWarn) 
                System.err.println("WARNING: The SharedBus "+scopename+"."+name+
                               " requested from Verilog was not found the the"+
                               " Java.");
        } else {
            VerilogSharedBus bus = (VerilogSharedBus) busArray.get(foundIndex);
            bus.connectToVerilog(handle,mode);
            if (debug) {
                System.out.println("VI: Connecting Pin "+scopename+"."+name+
                                   " size= "+busSize+
                                   " using handle "+handle+" mode= "+mode+
                                   " index= "+foundIndex);
            }
        }
        return foundIndex;
    }
    
    public static void pinChange(int index, int[] aval, int[] bval, 
                                 long timeInFemtoSecs) {
        if (index < 0) {
            System.out.println("Pin Change Ignored, unknown pin.");
            return;
        }
        VerilogSharedBus bus = (VerilogSharedBus) busArray.get(index);
        long time = 
            SharedBus.convert(timeInFemtoSecs, SharedBus.fsec);
        if (debug) {
            System.out.print("VI: Pin "+bus.getFullname()+
                             " @ "+timeInFemtoSecs+" equals:\n\t");
            for (int loop=aval.length-1;loop>=0;loop--) {
                System.out.print(aval[loop]+" ");
            }
            System.out.print(" aval.length= "+aval.length+"\n\t");
            for (int loop=bval.length-1;loop>=0;loop--) {
                System.out.print(bval[loop]+" ");
            }
            System.out.println(" bval.length= "+bval.length);
            System.out.println(timeInFemtoSecs+" : [Java] getTime= "+getTime()+" time= "+time+" from "+timeInFemtoSecs);
        }
        
        if ((time - getTime()) < 0) {
            //This is caused by rounding in the conversion from femto
            //to dsim units.  In this particular case, round up so that
            //the event does not fire in the past.
            //One DSim unit should be negligible...
            bus.set(driver, aval, bval, 0);
        } else {
            bus.set(driver, aval, bval, time - getTime()); //computes delay
        }
    }

    /** Hand of God Methods that use PLI calls to directly affect pins
     * in verilog.  NOTE:  these PLI methods are not terribly efficient.
     **/

    public static void forceSignal(String fullName, Data value, long delay) 
        throws SignalNotFoundException {
        long handle = get_handle_by_name(fullName); 
        if (handle <=0) 
            throw new SignalNotFoundException("Could not find signal: "+fullName);
        System.out.println("Forcing "+fullName+" at "+SharedBus.getSecs(getTime()+delay));
        forceBus(handle, value, delay);
    }
    public static void forceSignal(String fullName, Data value) 
        throws SignalNotFoundException {
            forceSignal(fullName, value, 0);
    }
    
    public static void releaseSignal(String fullName, long delay) 
        throws SignalNotFoundException {
        long handle = get_handle_by_name(fullName); 
        if (handle <=0) 
            throw new SignalNotFoundException("Could not find signal: "+fullName);
        Data fake = new Data(1);
        fake.set(0);
        System.out.println("Releasing "+fullName+" at "+(SharedBus.getSecs(getTime()+delay)/1e-9));
        releaseBus(handle, fake,delay);
    }
    
    public static void releaseSignal(String fullName) 
        throws SignalNotFoundException {
            releaseSignal(fullName, 0);
    }
    
    public static Data getValue(String scope, String name) {
    
        long handle = get_handle_by_name(scope+"."+name);
        return getValue(handle, get_size_by_handle(handle));
    }

    private static Data getValue(long handle, int size) {
        Data d = new Data(size);
        getValue(handle, d.getAval(), d.getBval());
        return d;
    }
    
    public static VerilogSharedBus createBus(String scopename, String name, 
                                             Sigscan sigscan, DebugOpts opts,
                                             int mode) 
                                             throws SignalNotFoundException {
        long handle = get_handle_by_name(scopename+"."+name);
        int size = get_size_by_handle(handle);
        VerilogSharedBus bus = new VerilogSharedBus(scopename, name, sigscan, opts, size);
        int index = connectPin(scopename, name, size, handle, mode);
        if (index < 0) 
            throw new SignalNotFoundException("Could not find pin "+scopename+"."+name);
        bus.connectToVerilog(handle, mode);
        doSignalCallback(index, handle, mode);
        bus.set(driver,getValue(handle, size), 0);
        return bus;
    }

    /** Syncs the Java Master Seed with Verilog **/
    private static long masterSeed = 0;

    public static long getJavaSeed() { return masterSeed; }

    public static void setMasterSeed(long seed) { masterSeed = seed; }

    /** New methods for connecting a java class directly into verilog **/

    public static final HashMap infoMap= new HashMap();
    
    public static void newDevice(String classname, String scope, long handle) {
        infoMap.put(new Long(handle),new DeviceInfo(classname, scope));
    }
    
    public static void finishDevice(long handle) {
        DeviceInfo info = (DeviceInfo) infoMap.get(new Long(handle));
        for(int loop=0;loop<handlers.size();loop++)
            ((DeviceHandler)handlers.get(loop)).deviceInstantiated(info);
        
    }

    public static int newPortDefinition(long scope_handle, String name, 
                                      long handle, int mode, int size) {
        DeviceInfo info = (DeviceInfo) infoMap.get(new Long(scope_handle));
        assert(info != null);
        VerilogSharedBus newPort = new VerilogSharedBus(
                info.getScopename(), name, null, new DebugOpts(), size);
        newPort.connectToVerilog(handle, mode);
        info.addPort(newPort);
        int index = findIndex(info.getScopename(), name);
        assert(index >= 0);
        return index;
    }

    public static void newStringParameter(long scope_handle, String name,
                                         String value) {
        DeviceInfo info = (DeviceInfo) infoMap.get(new Long(scope_handle));
        assert(info != null);
        info.addParameter(name, value);
    }
    
    public static void newIntParameter(long scope_handle, String name,
                                      int value) {
        DeviceInfo info = (DeviceInfo) infoMap.get(new Long(scope_handle));
        assert(info != null);
        info.addParameter(name, new Integer(value));
    }
    
    public static void newRealParameter(long scope_handle, String name,
                                       double value) {
        DeviceInfo info = (DeviceInfo) infoMap.get(new Long(scope_handle));
        assert(info != null);
        info.addParameter(name, new Double(value));
    }

    public static void addNewDeviceHandler(DeviceHandler handler) {
        handlers.add(handler);
    }
    
}

