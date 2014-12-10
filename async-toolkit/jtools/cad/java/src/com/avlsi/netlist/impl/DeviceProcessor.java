/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.NoSuchElementException;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.DeviceProcessorInterface;

/**
 * Processes devices in a netlist in a simple way.  This class is ideal for
 * device transformations that is one-to-one and stateless.  If the
 * transformation of a device is dependent on other devices, deletes devices or
 * adds devices, then this class should not be used.
 */
public class DeviceProcessor implements AbstractNetlist, Visitor {
    protected AbstractNetlist netlist;
    protected final LinkedList processors;
    private AbstractDevice result;

    public void genericTransistor(final HierName name,
                                  final AbstractNode drain,
                                  final AbstractNode gate,
                                  final AbstractNode source,
                                  final AbstractNode bulk,
                                  final double length,
                                  final double width,
                                  final String type,
                                  final Map parameters) {
        DeviceProcessorInterface.Transistor device =
            new DeviceProcessorInterface.Transistor(name, drain, gate,
                                                    source, bulk, length,
                                                    width, type, parameters);
        for (Iterator i = processors.iterator(); i.hasNext(); ) {
            DeviceProcessorInterface dpi = (DeviceProcessorInterface) i.next();
            device = dpi.processTransistor(device);
        }
        final DeviceProcessorInterface.Transistor d = device;
        result = new AbstractDevice() {
            public void accept(Visitor v) {
                v.genericTransistor(d.name, d.drain, d.gate, d.source, d.bulk,
                                    d.length, d.width, d.type, d.parameters);
            }
        };
    }

    public void genericDiode(final HierName name,
                             final AbstractNode positive,
                             final AbstractNode negative,
                             final double length,
                             final double width,
                             final double perimeter,
                             final double area,
                             final String type,
                             final Map parameters) {
        DeviceProcessorInterface.Diode device =
            new DeviceProcessorInterface.Diode(name, positive, negative,
                                               length, width, perimeter,
                                               area, type, parameters);
        for (Iterator i = processors.iterator(); i.hasNext(); ) {
            DeviceProcessorInterface dpi = (DeviceProcessorInterface) i.next();
            device = dpi.processDiode(device);
        }
        final DeviceProcessorInterface.Diode d = device;
        result = new AbstractDevice() {
            public void accept(Visitor v) {
                v.genericDiode(d.name, d.positive, d.negative, d.length,
                               d.width, d.perimeter, d.area, d.type,
                               d.parameters);
            }
        };
    }

    public void genericResistor(final HierName name,
                                final AbstractNode node1,
                                final AbstractNode node2,
                                final double conductance,
                                final Map parameters) {
        DeviceProcessorInterface.Resistor device =
            new DeviceProcessorInterface.Resistor(name, node1, node2,
                                                  conductance, parameters);
        for (Iterator i = processors.iterator(); i.hasNext(); ) {
            DeviceProcessorInterface dpi = (DeviceProcessorInterface) i.next();
            device = dpi.processResistor(device);
        }
        final DeviceProcessorInterface.Resistor d = device;
        result = new AbstractDevice() {
            public void accept(Visitor v) {
                v.genericResistor(d.name, d.node1, d.node2, d.conductance,
                                  d.parameters);
            }
        };
    }

    public void genericCapacitor(final HierName name,
                                 final AbstractNode positive,
                                 final AbstractNode negative,
                                 final double capacitance,
                                 final Map parameters) {
        DeviceProcessorInterface.Capacitor device =
            new DeviceProcessorInterface.Capacitor(name, positive, negative,
                                                   capacitance, parameters);
        for (Iterator i = processors.iterator(); i.hasNext(); ) {
            DeviceProcessorInterface dpi = (DeviceProcessorInterface) i.next();
            device = dpi.processCapacitor(device);
        }
        final DeviceProcessorInterface.Capacitor d = device;
        result = new AbstractDevice() {
            public void accept(Visitor v) {
                v.genericCapacitor(d.name, d.positive, d.negative,
                                   d.capacitance, d.parameters);
            }
        };
    }

    public void genericInductor(final HierName name,
                                final AbstractNode positive,
                                final AbstractNode negative,
                                final double inductance,
                                final Map parameters) {
        DeviceProcessorInterface.Inductor device =
            new DeviceProcessorInterface.Inductor(name, positive, negative,
                                                  inductance, parameters);
        for (Iterator i = processors.iterator(); i.hasNext(); ) {
            DeviceProcessorInterface dpi = (DeviceProcessorInterface) i.next();
            device = dpi.processInductor(device);
        }
        final DeviceProcessorInterface.Inductor d = device;
        result = new AbstractDevice() {
            public void accept(Visitor v) {
                v.genericInductor(d.name, d.positive, d.negative, d.inductance,
                                  d.parameters);
            }
        };
    }

    public void subcircuitCall(final HierName name,
                               final AbstractNetlist circuit,
                               final AbstractNodeIterator nodes,
                               final Map parameters) {
        DeviceProcessorInterface.Call device =
            new DeviceProcessorInterface.Call(name, circuit,
                                              nodes, parameters);
        for (Iterator i = processors.iterator(); i.hasNext(); ) {
            DeviceProcessorInterface dpi = (DeviceProcessorInterface) i.next();
            device = dpi.processCall(device);
        }
        final DeviceProcessorInterface.Call d = device;
        result = new AbstractDevice() {
            public void accept(Visitor v) {
                v.subcircuitCall(d.name, d.circuit, d.nodes, d.parameters);
            }
        };
    }

    public DeviceProcessor(final AbstractNetlist netlist) {
        this.netlist = netlist;
        this.processors = new LinkedList();
    }

    public void appendProcessor(final DeviceProcessorInterface dpi) {
        processors.addLast(dpi);
    }

    public void prependProcessor(final DeviceProcessorInterface dpi) {
        processors.addFirst(dpi);
    }

    public void setNetlist(final AbstractNetlist netlist) {
        this.netlist = netlist;
        for (Iterator i = processors.iterator(); i.hasNext(); ) {
            DeviceProcessorInterface dpi = (DeviceProcessorInterface) i.next();
            dpi.processCell(getName());
        }
    }

    public AbstractNodeIterator getInputNodes() {
        return netlist.getInputNodes();
    }

    public AbstractNodeIterator getOutputNodes() {
        return netlist.getOutputNodes();
    }

    public HierName getName() {
        return netlist.getName();
    }

    public AbstractNodeIterator getNodes() {
        return netlist.getNodes();
    }

    public AbstractDeviceIterator getDevices() {
        final AbstractDeviceIterator devices = netlist.getDevices();
        return new AbstractDeviceIterator() {
            public boolean hasNext() {
                return devices.hasNext();
            }
            public AbstractDevice next() {
                final AbstractDevice d = devices.next();
                d.accept(DeviceProcessor.this);
                return result;
            }
        };
    }

    public AbstractNetlistIterator getSubcircuits() {
        return netlist.getSubcircuits();
    }
}
