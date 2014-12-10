/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.io.PrintWriter;
import java.io.Writer;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.NetlistFormatter;

public class SkillEmitter extends NetlistFormatter implements Visitor {
    private PrintWriter w;

    public SkillEmitter(Writer w) {
        this.w = new PrintWriter(w);
    }

    private String stringNode(AbstractNode node) {
        return "\"" + node.getCanonicalName().getCadenceString() + "\"";
    }

    private void keypair(Map parameters) {
        Iterator i = parameters.entrySet().iterator();

        while (i.hasNext()) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            Double val = (Double) entry.getValue();
            w.print(" beginlist " + key + " " + val + " endlist");
        }
    }

    public void genericTransistor(final HierName name,
                                  final AbstractNode drain,
                                  final AbstractNode gate,
                                  final AbstractNode source,
                                  final AbstractNode bulk,
                                  final double length,
                                  final double width,
                                  final String type,
                                  final Map parameters) {
        w.print("beginlist transistor " +
                "M" + name.getCadenceString() + " " +
                stringNode(drain) + " " +
                stringNode(gate) + " " +
                stringNode(source) + " " +
                stringNode(bulk) + " " +
                type + " beginlist " +
                "beginlist width " + width + " endlist " +
                "beginlist length " + length + " endlist"
                );
        
        keypair(parameters);

        w.println(" endlist endlist");
    }

    public void genericDiode(final HierName name,
                             final AbstractNode positive,
                             final AbstractNode negative,
                             final double length,
                             final double width,
                             final double perimeter,
                             final double area,
                             final String type,
                             final Map parameters) { }

    public void genericResistor(final HierName name,
                                final AbstractNode node1,
                                final AbstractNode node2,
                                final double conductance,
                                final Map parameters) { }

    public void genericCapacitor(final HierName name,
                                 final AbstractNode positive,
                                 final AbstractNode negative,
                                 final double capacitance,
                                 final Map parameters) { }

    public void genericInductor(final HierName name,
                                final AbstractNode positive,
                                final AbstractNode negative,
                                final double inductance,
                                final Map parameters) { }

    public void subcircuitCall(final HierName name,
                               final AbstractNetlist circuit,
                               final AbstractNodeIterator nodes,
                               final Map parameters) {
        w.print("beginlist subcell " + name.getCadenceString());

        for (AbstractNodeIterator formal = circuit.getInputNodes();
             formal.hasNext() && nodes.hasNext();) {
            w.print(" beginlist");
            w.print(" " + stringNode(nodes.next()));
            w.print(" " + stringNode(formal.next()));
            w.print(" endlist");
        }

        for (AbstractNodeIterator formal = circuit.getOutputNodes();
             formal.hasNext() && nodes.hasNext();) {
            w.print(" beginlist");
            w.print(" " + stringNode(nodes.next()));
            w.print(" " + stringNode(formal.next()));
            w.print(" endlist");
        }

        keypair(parameters);

        w.println(" endlist");
    }

    public void devicesBegin() {
        w.println("beginlist");
    }

    public void devicesEnd() {
        w.println("endlist");
    }

    /*
    public void subcktBegin(final AbstractNetlist netlist) {

        w.print(".SUBCKT " + netlist.getName().getCadenceString());

        AbstractNodeIterator i;
        i = netlist.getOutputNodes();
        while (i.hasNext()) {
            w.print(" " + stringNode(i.next()));
        }

        i = netlist.getInputNodes();
        if (i.hasNext()) {
            w.print(" /");
            while (i.hasNext()) {
                w.print(" " + stringNode(i.next()));
            }
        }

        w.println();
    }

    public void subcktEnd(final AbstractNetlist netlist) {
        w.println(".ENDS");
    }
    */
}
