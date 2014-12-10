/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.util.Collection;
import java.util.Map;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;

/**
 * Extend this class by override one or more of the process functions.  This is
 * meant to be used in conjunction with DeviceProcessor.
 */
public class DeviceProcessorInterface {
    public static class Transistor {
        public HierName name;
        public AbstractNode drain, gate, source, bulk;
        public double length, width;
        public String type;
        public Map parameters;
        public Transistor(final HierName name, final AbstractNode drain,
                          final AbstractNode gate, final AbstractNode source,
                          final AbstractNode bulk, final double length,
                          final double width, final String type,
                          final Map parameters) {
            this.name = name;
            this.drain = drain;
            this.gate = gate;
            this.source = source;
            this.bulk = bulk;
            this.length = length;
            this.width = width;
            this.type = type;
            this.parameters = parameters;
        }
    }

    public Transistor processTransistor(final Transistor device) {
        return device;
    }

    public static class Diode {
        public HierName name;
        public AbstractNode positive, negative;
        public double length, width;
        public double perimeter, area;
        public String type;
        public Map parameters;
        public Diode(final HierName name, final AbstractNode positive,
                     final AbstractNode negative, final double length,
                     final double width, final double perimeter,
                     final double area, final String type,
                     final Map parameters) {
            this.name = name;
            this.positive = positive;
            this.negative = negative;
            this.length = length;
            this.width = width;
            this.perimeter = this.perimeter;
            this.area = area;
            this.type = type;
            this.parameters = parameters;
        }
    }

    public Diode processDiode(final Diode device) {
        return device;
    }

    public static class Resistor {
        public HierName name;
        public AbstractNode node1, node2;
        public double conductance;
        public Map parameters;
        public Resistor(final HierName name, final AbstractNode node1,
                        final AbstractNode node2, final double conductance,
                        final Map parameters) {
            this.name = name;
            this.node1 = node1;
            this.node2 = node2;
            this.conductance = conductance;
            this.parameters = parameters;
        }
    }

    public Resistor processResistor(final Resistor device) {
        return device;
    }

    public static class Capacitor {
        public HierName name;
        public AbstractNode positive, negative;
        public double capacitance;
        public Map parameters;
        public Capacitor(final HierName name, final AbstractNode positive,
                         final AbstractNode negative, final double capacitance,
                         final Map parameters) {
            this.name = name;
            this.positive = positive;
            this.negative = negative;
            this.capacitance = capacitance;
            this.parameters = parameters;
        }
    }

    public Capacitor processCapacitor(final Capacitor device) {
        return device;
    }

    public static class Inductor {
        public HierName name;
        public AbstractNode positive, negative;
        public double inductance;
        public Map parameters;
        public Inductor(final HierName name, final AbstractNode positive,
                        final AbstractNode negative, final double inductance,
                        final Map parameters) {
            this.name = name;
            this.positive = positive;
            this.negative = negative;
            this.inductance = inductance;
            this.parameters = parameters;
        }
    }

    public Inductor processInductor(final Inductor device) {
        return device;
    }

    public static class Call {
        public HierName name;
        public AbstractNetlist circuit;
        public AbstractNodeIterator nodes;
        public Map parameters;
        public Call(final HierName name, final AbstractNetlist circuit,
                    final AbstractNodeIterator nodes, final Map parameters) {
            this.name = name;
            this.circuit = circuit;
            this.nodes = nodes;
            this.parameters = parameters;
        }
    }

    public Call processCall(final Call device) {
        return device;
    }

    public void processCell(final HierName cell) {
    }
}
