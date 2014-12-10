/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

import com.avlsi.tools.tsim.SharedBus;
import com.avlsi.tools.tsim.DelayedSetEvent;
import com.avlsi.tools.tsim.BusDriver;
import com.avlsi.tools.tsim.ForceDriver;
import com.avlsi.tools.tsim.Data;
import com.avlsi.tools.tsim.NativeBusDriver;
import com.avlsi.tools.dsim.DigitalScheduler;

import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.SigscanException;
import com.avlsi.tools.sigscan.DebugOpts;

import java.io.IOException;

import com.avlsi.util.debug.Debug;


/**
 * Class to share SharedBuses with Verilog.
 * 
 * The name of the VerilogSharedBus should match exactly with the name
 * of the wire/reg in verilog.
 *
 * For Java to be able to drive verilog, it needs to attach itself to a
 * signal that has a wire AND a reg.  The reg is used to store the new
 * values put into verilog by Java.
 *
 * For this reason, a set of nodes being driven by Java are usually put into
 * their own module- and then this 'stub' module can then easily interact
 * with other modules in verilog.
 * 
 * @see com.avlsi.tools.tsim.verilog.VpiInterface
 * @author Dan Daly
 * @version $Date$
 **/

public class VerilogSharedBus extends SharedBus {

    private int io_mode = -1;
    private long write_handle = 0;

    /** Am I connected to the verilog?  When false, this bus acts
     * as a regular shared bus
     **/
    private boolean connected=false;
    /**Builds a new VerilogSharedBus, using the default connection
     * Does NOT check if the signal exists in Verilog.
     * @param scope Scope of the bus
     * @param name Name of the pin (supplied by Denali)
     * @param sigscan The sigscan database to use to log
     * @param opts The DebugOpts used, set setLogBuses to enable logging
     * @param busSize Size of the bus
     * */
   public VerilogSharedBus(String scope, String name,
                            Sigscan sigscan, DebugOpts opts,
                            int busSize, boolean connect) {
       super(scope, name, sigscan, opts, busSize);
       //System.out.println("Building "+getFullname());
       VpiInterface.addBus(this);

   }

    /**Builds a new VerilogSharedBus, using the default connection
    * @param scope Scope of the bus
    * @param name Name of the pin (supplied by Denali)
    * @param sigscan The sigscan database to use to log
    * @param opts The DebugOpts used, set setLogBuses to enable logging
    * @param busSize Size of the bus
    * */
    public VerilogSharedBus(String scope, String name,
                        Sigscan sigscan, DebugOpts opts,
                        int busSize) {
        this(scope, name, sigscan, opts, busSize, true);
    }

    /**Builds a new VerilogSharedBus, using the default connection
    * @param scope Scope of the bus
    * @param name Name of the pin (supplied by Denali)
    * @param sigscan The sigscan database to use to log
    * @param opts The DebugOpts used, set setLogBuses to enable logging
    * @param busSize Size of the bus
    * @param conn NOT USED
    * */
    public VerilogSharedBus(String scope, String name,
                        Sigscan sigscan, DebugOpts opts,
                        int busSize, VerilogConnection conn) {
        this(scope, name, sigscan, opts, busSize, true);
    }

    public void connectToVerilog(long handle,int mode) {
        //System.out.println(getFullname()+" Connected to verilog mode= "+mode);
        io_mode = mode;
        if (mode == VpiInterface.JV_OUTPUT) {
            this.write_handle = handle;
        }
        connected = true;
    }

    public void set(BusDriver driver, Data pending, long delay) {
        super.set(driver, pending, delay);
        if (connected && (!(driver instanceof NativeBusDriver))) {
             if (write_handle == 0) {
                System.out.println(
                DigitalScheduler.get().getTime()+
                " : WARNING: set command on verilog bus\n\t"+
                getFullname()+" = "+
                pending+"\nThis pin is read only\n"+
                "This means that Verilog and Java pins may be"+
                " out of sync, enabling VerilogSharedBus logging");
                getDebugOpts().setLogBuses(true);
                return;
            }
            if (driver.equals(forceDriver)) {
                VpiInterface.forceBus(write_handle, pending, delay);
            } else {
                VpiInterface.setBus(write_handle, pending, delay);
            }
        } else if ((!connected)  && (DigitalScheduler.get().getTime() == 0)) {
            //This bus -may- exist, but just hasn't been found
            //in the verilog yet, so we wait
            DigitalScheduler.get().addEvent(
                    new DelayedSetEvent(this, driver, pending, delay, 1));
        }
    }

    /** Slightly optimized recordChange because a VSB can only
     * have one driver.
     **/
    public void recordChange(BusDriver drv, Data newdata, long time) { 
        boolean valChange=false;
        boolean rose = false, fell = false;
        int ret = data.setTo(newdata);
        valChange = ret != 0;
        rose = ret == 1;
        fell = ret == -1;
        reportToWatchers(time,rose, fell);
        notifyAll(rose, fell);
    }
    
    public void release() {
        Data fake = new Data(1);
        VpiInterface.releaseBus(write_handle,fake,0L);
    }

    public boolean isInput() { return (io_mode == VpiInterface.JV_INPUT); }
    public boolean isOutput() { return (io_mode == VpiInterface.JV_OUTPUT); }
}

