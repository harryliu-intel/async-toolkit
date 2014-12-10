/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.Visitor;

public class NetlistFormatter {
    public void subcktBegin(final AbstractNetlist circuit) {}
    public void subcktEnd(final AbstractNetlist circuit) {}

    public void deviceBegin(final AbstractDevice device) {}
    public void deviceEnd(final AbstractDevice device) {}

    public void devicesBegin() {}
    public void devicesEnd() {}

    public void format(final AbstractNetlist netlist, final Visitor visitor,
                       final boolean recursive) {
        subcktBegin(netlist);

        if (recursive) {
            for (AbstractNetlistIterator i = netlist.getSubcircuits();
                 i.hasNext();) {
                AbstractNetlist subckt = i.next();
                format(subckt, visitor);
            }
        }

        devicesBegin();
        for (AbstractDeviceIterator i = netlist.getDevices(); i.hasNext();) {
            AbstractDevice device = i.next();
            deviceBegin(device);
            device.accept(visitor);
            deviceEnd(device);
        }
        devicesEnd();
        subcktEnd(netlist);
    }

    public void format(final AbstractNetlist netlist, final Visitor visitor) {
        format(netlist, visitor, true);
    }
}
