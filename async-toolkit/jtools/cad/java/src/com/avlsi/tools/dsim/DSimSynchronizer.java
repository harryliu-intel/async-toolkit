/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.dsim;
import com.avlsi.tools.tsim.AbstractDevice;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Class for synchronizing timestep simulators with DSim's scheduler.  Because DSim time
 * ticks away in meaningless DSim time units (where 100 is the time expected for a prs transistion)
 * the conversion between these units and rewal time is specified
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class DSimSynchronizer extends AbstractDevice {

    private int period;
    private final int ck_dsim;  
    private final double ck_realtime;
    boolean stop = false;
    private ArrayList devices;
    /**
     * Constructor.
     **/
    public DSimSynchronizer(final int ck_dsim, final double ck_realtime) {
        this(ck_dsim, ck_realtime,ck_dsim/8);
    }
    
    public DSimSynchronizer(final int ck_dsim,
                            final double ck_realtime,
                            final int period) {
        this.ck_dsim = ck_dsim;
        this.ck_realtime = ck_realtime;
        this.period = period;
        this.devices = new ArrayList();
    }

    public double DSim_to_RealTime(long dsim_time) {
        return (((double)dsim_time*ck_realtime)/ck_dsim);
    }

    public void go() throws InterruptedException {
        while (!stopped()) {
            try {
                for (final Iterator i = devices.iterator();i.hasNext();) {
                    Runnable r = (Runnable) i.next();
                    r.run(DSim_to_RealTime(DSim.get().getTime()));
                    //Run each instance to DSim time equiv
                }
                
            } catch (Exception e) {
                System.out.println("Exception called, stopping, :"+e.getMessage());
                stop = true;
                break;
            }
            
            new WakeAt(DSim.get().getTime()+period,this).sleepTil();
        }
        //cleanup called after go quits
        } 

        public boolean stopped() {
            return stop;
        }

        public void stop() {
            stop = true;
        }
        
        public void setPeriod(int p) {
            this.period = p;
        }

        public int getPeriod() { return period;}

        public void addDevice(Runnable r) {
            devices.add(r);
        }
        
        public interface Runnable {
            void run(double time) throws Exception;
        }
}

