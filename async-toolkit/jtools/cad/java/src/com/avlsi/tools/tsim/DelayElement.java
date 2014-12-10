/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

/**
 * Class for modelling a delay element in a synchronous system
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class DelayElement implements BusWatcher, BusDriver {

    private final SharedBus output;
    private long delay;
    
    /** Creates a delay element that will propagate changes in the input
     * bus to the output bus with delay <code>delayInDsimUnits</code>
     * @param input The input bus
     * @param output The output bus
     * @param delayInDsimUnits The amount of delay to introduce
     **/
    public DelayElement(SharedBus input, SharedBus output,
                        long delayInDsimUnits) {
        input.addWatch(this);
        this.output = output;
        this.delay = delayInDsimUnits;
    }

    /** Creates a delay element that will propagate changes in the input
     * bus to the output bus with delay 
     * <code>timeDelay</code>*<code>units</code>.
     * @param input The input bus
     * @param output The output bus
     * @param timeDelay delay from change in input to change in output
     * @param units units of the time delay( SharedBus.sec, etc)
     */
    public DelayElement(SharedBus input, SharedBus output,
                        long timeDelay, long units) {
        this(input, output, SharedBus.convert(timeDelay, units));
    }           

    public void busChanged(SharedBus input, long time) {
        output.set(this, input.get(), delay);
    }

    public void setDelay(long delay) { this.delay = delay; }

    public void setDelay(long timeDelay, long units) { 
        setDelay(SharedBus.convert(timeDelay, units));
    }
}

