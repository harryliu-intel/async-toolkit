/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist;

import java.util.Iterator;
import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.AbstractDeviceIterator;

/**
   Interface to a netlist.  A netlist contains a set of nets, which are called
   nodes, and a set of devices, such as transistors, resistors and capacitors
   that are connected by the nets.  A netlist is hierachical, with possibly
   nesting subcircuits.
 */
public interface AbstractNetlist {
    /**
       Gets an iterator that iterates over all input nodes.
       @return an AbstractNodeIterator over input nodes
     */
    AbstractNodeIterator getInputNodes();

    /**
       Gets an iterator that iterates over all output nodes.
       @return an AbstractNodeIterator over output nodes
     */
    AbstractNodeIterator getOutputNodes();

    /**
       Gets the name of this subcircuit.
       @return name of this subcircuit
     */
    HierName getName();

    /**
       Gets an iterator that iterates over all nodes in the netlist.
       @return an AbstractNodeIterator over all nodes
     */
    AbstractNodeIterator getNodes();

    /**
       Gets an iterator that iterates over all devices in the netlist.
       @return an AbstractDeviceIterator over all devices
     */
    AbstractDeviceIterator getDevices();

    /**
       Gets an iterator that iterates over all subcircuits defined in the
       netlist.
       @return an AbstractNetlistIterator over all subcircuits
     */
    AbstractNetlistIterator getSubcircuits();
}
