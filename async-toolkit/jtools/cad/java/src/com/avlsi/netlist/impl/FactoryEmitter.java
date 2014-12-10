/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.io.PrintWriter;
import java.io.Writer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.NetlistFormatter;
import com.avlsi.util.text.PrintfFormat;

public class FactoryEmitter extends NetlistFormatter implements Visitor {
    private final CDLFactoryInterface out;
    private final PrintfFormat fmt;

    public FactoryEmitter(final CDLFactoryInterface out) {
        this(out, null);
    }

    public FactoryEmitter(final CDLFactoryInterface out,
                          final PrintfFormat fmt) {
        this.out = out;
        this.fmt = fmt;
    }

    private HierName node(final AbstractNode n) {
        return n.getCanonicalName();
    }

    private HierName[] node(final AbstractNodeIterator nodes) {
        final Collection result = new ArrayList();
        while (nodes.hasNext()) {
            result.add(node(nodes.next()));
        }
        return (HierName[]) result.toArray(new HierName[0]);
    }

    private String[] stringNode(final AbstractNodeIterator nodes) {
        final Collection result = new ArrayList();
        while (nodes.hasNext()) {
            result.add(node(nodes.next()).getCadenceString());
        }
        return (String[]) result.toArray(new String[0]);
    }

    private CDLLexer.InfoToken toToken(final double val) {
        return new CDLLexer.SimpleToken(new Double(val));
    }

    private CDLLexer.InfoToken toToken(final Double val) {
        return new CDLLexer.SimpleToken(val, fmt == null ? val.toString() :
                                             fmt.sprintf(val));
    }

    private Map keypair(Map parameters) {
        final Map result = new LinkedHashMap();
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            Double val = (Double) entry.getValue();
            result.put(key, toToken(val));
        }
        return result;
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
        out.makeTransistor(name, type, node(source), node(drain),
                           node(gate), node(bulk),
                           toToken(width), toToken(length),
                           keypair(parameters), NullEnvironment.getInstance());

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
                                final Map parameters) {
        out.makeResistor(name, node(node1), node(node2),
                         toToken(1 / conductance), keypair(parameters),
                         NullEnvironment.getInstance());
    }

    public void genericCapacitor(final HierName name,
                                 final AbstractNode positive,
                                 final AbstractNode negative,
                                 final double capacitance,
                                 final Map parameters) {
        out.makeCapacitor(name, node(positive), node(negative),
                          toToken(capacitance), keypair(parameters),
                          NullEnvironment.getInstance());
    }

    public void genericInductor(final HierName name,
                                final AbstractNode positive,
                                final AbstractNode negative,
                                final double inductance,
                                final Map parameters) { }

    public void subcircuitCall(final HierName name,
                               final AbstractNetlist circuit,
                               final AbstractNodeIterator nodes,
                               final Map parameters) {
        out.makeCall(name, circuit.getName().getCadenceString(),
                     node(nodes), keypair(parameters),
                     NullEnvironment.getInstance());
    }

    public void subcktBegin(final AbstractNetlist netlist) {
        out.beginSubcircuit(netlist.getName().getCadenceString(),
                            stringNode(netlist.getInputNodes()),
                            stringNode(netlist.getOutputNodes()),
                            Collections.EMPTY_MAP,
                            NullEnvironment.getInstance());
    }

    public void subcktEnd(final AbstractNetlist netlist) {
        out.endSubcircuit(netlist.getName().getCadenceString(),
                          NullEnvironment.getInstance());
    }
}
