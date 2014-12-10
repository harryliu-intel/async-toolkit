/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist;

import java.util.Collection;
import java.util.Map;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractNetlist;

/**
   Visitor interface used by AbstractDevice.
 */
public interface Visitor {
    /**
       Called by implementations of AbstractDevice that implement a transistor.
       Parameters is a table of key-value pairs containing additional
       parameters.  The name of the transistor must be unique for every
       instance within a subcircuit.
       @param name Name of the transistor
       @param drain Drain node
       @param gate Gate node
       @param source Source node
       @param bulk Substrate node
       @param length Length in meters
       @param width Width in meters
       @param type Type of transistor
       @param parameters A Map from String to Double that captures additional
       parameters
     */
    void genericTransistor(final HierName name,
                           final AbstractNode drain,
                           final AbstractNode gate,
                           final AbstractNode source,
                           final AbstractNode bulk,
                           final double length,
                           final double width,
                           final String type,
                           final Map parameters);

    /**
       Called by implementations of AbstractDevice that implement a diode.
       Parameters is a table of key-value pairs containing additional
       parameters.  The name of the transistor must be unique for every
       instance within a subcircuit.
       @param name Name of the diode
       @param positive The positive terminal
       @param negative The negative terminal
       @param length Length in meters
       @param width Width in meters
       @param perimeter Perimeter in meters
       @param area Area in square meters
       @param type Type of diode 
       @param parameters A Map from String to Double that captures additional
       parameters
     */
    void genericDiode(final HierName name,
                      final AbstractNode positive,
                      final AbstractNode negative,
                      final double length,
                      final double width,
                      final double perimeter,
                      final double area,
                      final String type,
                      final Map parameters);

    /**
       Called by implementations of AbstractDevice that implement a resistor.
       Parameters is a table of key-value pairs containing additional
       parameters.  The name of the transistor must be unique for every
       instance within a subcircuit.
       @param name Name of the resistor
       @param node1 First terminal
       @param node2 Second terminal
       @param conductance Conductance in mho
       @param parameters A Map from String to Double that captures additional
       parameters
     */
    void genericResistor(final HierName name,
                         final AbstractNode node1,
                         final AbstractNode node2,
                         final double conductance,
                         final Map parameters);

    /**
       Called by implementations of AbstractDevice that implement a capacitor.
       Parameters is a table of key-value pairs containing additional
       parameters.  The name of the transistor must be unique for every
       instance within a subcircuit.
       @param name Name of the capacitor
       @param positive The positive terminal
       @param negative The negative terminal
       @param capacitance Capacitance in farads
       @param parameters A Map from String to Double that captures additional
       parameters
     */
    void genericCapacitor(final HierName name,
                          final AbstractNode positive,
                          final AbstractNode negative,
                          final double capacitance,
                          final Map parameters);

    /**
       Called by implementations of AbstractDevice that implement an inductor.
       Parameters is a table of key-value pairs containing additional
       parameters.  The name of the inductor must be unique for every
       instance within a subcircuit.
       @param name Name of the inductor 
       @param positive The positive terminal
       @param negative The negative terminal
       @param inductance Inductance in henrys
       @param parameters A Map from String to Double that captures additional
       parameters
     */
    void genericInductor(final HierName name,
                         final AbstractNode positive,
                         final AbstractNode negative,
                         final double inductance,
                         final Map parameters);

    /**
       Called by implementations of AbstractDevice that implement a subcircuit
       call.
       @param name Name of the instantiated cell. 
       @param circuit Circuit description
       @param nodes Input nodes in the same order as <code>circuit.getInputNodes</code> 
       would return, followed by output nodes in the same order as 
       <code>circuit.getOutputNodes</code> would return.
       @param parameters A Map from String to Double that captures additional
       parameters
       
     */
    void subcircuitCall(final HierName name,
                        final AbstractNetlist circuit,
                        final AbstractNodeIterator nodes,
                        final Map parameters);

}
