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

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class VerilogInterface implements NativeBusDriver{
   
    static {
        /** If VerilogInterface is EVER used, we need to
         * disable the SchedulerRunner, because verilog is
         * going to run the Scheduler.
         **/

        SchedulerRunner.get().setEnable(false);
    }
    
    protected static final Hashtable buses = new Hashtable();

    private static final VerilogInterface driver = new VerilogInterface();

    private native static void setVerilogBus(String name, String value, 
                                             long timeInFemtoSecs);
    
    /**
     * Constructor.
     **/
    private VerilogInterface() {}

    public static long queryNextEventTime() {
        long time = DigitalScheduler.get().getNextEventTime();
        long timeInFemtoSecs = 
            Math.round(SharedBus.getSecs(time)/SharedBus.fsec);

        return timeInFemtoSecs;
    }

    public static long evaluateJavaSimulatorUpTo(long timeInFemtoSecs) {
        long time = SharedBus.convert(timeInFemtoSecs, SharedBus.fsec);
        DigitalScheduler dsim = DigitalScheduler.get();
        //System.out.println("Cycling to "+(time+1)+" from "+timeInFemtoSecs);
        dsim.cycleTo(time+1); //Include the time given
        dsim.waitUntilStable();
        return queryNextEventTime();
    }
    
    public static void setBus(VerilogSharedBus bus, Data newval, long time) {
        //System.out.println("Setting "+bus.getFullname()+" to "+newval+
        //                   " @ "+time);
        setVerilogBus(bus.getFullname(), "0b"+newval.getStringData(),
                      Math.round(SharedBus.getSecs(time)/SharedBus.fsec));
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
        /*if (bus.getName().startsWith("i_rx_capture")) {
            System.out.println("Setting "+bus.getFullname()+" to "+value[0]+" @ "+time+" delay= "+(time - getTime()));
        }*/
        if ((time - getTime()) == -1) {
            //This is caused by rounding in the conversion from femto
            //to dsim units.  In this particular case, round up so that
            //the event does not fire in the past.
            //One DSim unit should be negligible...
            bus.set(driver, value, 0);//bus.getTime());
        } else
            bus.set(driver, value, time - getTime()); //computes delay
    }

    public static long getTime() { return DigitalScheduler.get().getTime(); }

    public static void registerBus(SharedBus bus) {
        buses.put(bus.getFullname(), bus);
    }

}

