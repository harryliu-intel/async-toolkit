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

package com.avlsi.tools.sigscan;

import com.avlsi.tools.dsim.DigitalScheduler;

/**
 * Class for recording events in JavaChannel into SST
 * @author Dan Daly
 * @version $Date$
 **/

public class ChannelSigscan extends Sigscan implements Runnable {
    
    private boolean stopped = false;
    
    private Thread cycler;
    
    public final void start() {
        if (cycler != null) return;
        cycler = new Thread(this);
        cycler.start();
    }
    
    public void run() {
        while (!stopped) { cycle(); }
    }

    public void cycle() { DigitalScheduler.get().cycle(); }

    public void stopSig() { stopped = true; }
    
    /**
     * Constructor.
     **/
    public ChannelSigscan(String tracename) 
            throws SigscanException {
        super(tracename);
        //moved to sigscan
        //Runtime.getRuntime().addShutdownHook( new Thread() {
        //    public void run() { stopSig();close(); } }  );

    }

    public ChannelSigscan(String tracename, boolean useOpen, int timeScale)
            throws SigscanException {
        super(tracename, useOpen, timeScale);
    }
}

