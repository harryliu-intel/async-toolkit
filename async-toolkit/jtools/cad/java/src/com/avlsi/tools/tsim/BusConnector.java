/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.NodeWatcher;
/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

class BusConnector implements NodeWatcher, BusWatcher, BusDriver{
    
    private final Node node;
    private final SharedBus bus;
    private final int position;
    

    /**
     * Constructor.
     **/
    public BusConnector(Node node, SharedBus bus, int position) {
        this.node = node;
        this.bus = bus;
        this.position = position;
    }
    
    public void nodeChanged(Node node, long time) {
        byte val = node.getValue();
    
        Data d = (Data) bus.get().clone();
        if (val == Node.VALUE_U) {
            d.setBval(position);
            d.setBit(position);
        } else {
            d.clearBval(position);
            if (val == 0) d.clearBit(position);
            else d.setBit(position);
        }
        //bus.recordChange(this,d,time);
        bus.set(this, d, 0);
    }

    public void busChanged(SharedBus bus, long time) {
        Data d = bus.get();
        byte newval = Node.VALUE_U;
        if (d.isX(position) || d.isZ(position)) newval = Node.VALUE_U;
        else {
            if (d.testBit(position)) newval = Node.VALUE_1;
            else newval = Node.VALUE_0;
        }
        node.scheduleDelay(newval,0);
    }

}

