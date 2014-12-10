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
 * Simple visitor implementation that can be inherited to easily override a
 * couple of methods.  All methods are empty.
 */
public class VisitorImpl implements Visitor {
    public void genericTransistor(final HierName name,
                                  final AbstractNode drain,
                                  final AbstractNode gate,
                                  final AbstractNode source,
                                  final AbstractNode bulk,
                                  final double length,
                                  final double width,
                                  final String type,
                                  final Map parameters) { }

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
                               final Map parameters) { }

}
