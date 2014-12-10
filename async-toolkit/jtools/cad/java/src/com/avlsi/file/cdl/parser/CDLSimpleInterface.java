/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.util.Map;

import com.avlsi.file.common.HierName;

/**
 * A simplied factory interface which makes the CDL parser easier to use.
 **/
public interface CDLSimpleInterface {
    /**
     * Called by the parser to make a resistor.
     * @param name Name of the resistor
     * @param n1 First terminal
     * @param n2 Second terminal
     * @param val Conductance in mho
     **/
    void makeResistor(HierName name, HierName n1, HierName n2, double val);

    /**
     * Called by the parser to make a capacitor.
     * @param name Name of the capacitor
     * @param npos Positive terminal
     * @param nneg Negative terminal
     * @param val Capacitance in farads
     **/
    void makeCapacitor(HierName name, HierName npos, HierName nneg, double val);

    /**
     * Called by the parser to make a transistor.
     * @param name Name of the transistor 
     * @param type Type of the transistor
     * @param ns Source
     * @param nd Drain
     * @param ng Gate
     * @param nb Bulk
     * @param w Width of the transistor in meters
     * @param l Length of the transistor in meters
     **/
    void makeTransistor(HierName name, int type, HierName ns, HierName nd,
                        HierName ng, HierName nb, double w, double l);

    /**
     * Called by the parser to make a diode.
     * @param name Name of the diode 
     * @param type Type of the diode
     * @param npos Positive terminal
     * @param nneg Negative terminal
     * @param w Width of the diode in meters
     * @param l Length of the diode in meters
     * @param a Area of the diode in square meters
     * @param p Perimeter of the diode in meters
     **/
    void makeDiode(HierName name, int type, HierName npos, HierName nneg,
                   double w, double l, double a, double p);

    /**
     * Called by the parser to make an inductor.
     * @param name Name of the inductor 
     * @param npos Positive terminal
     * @param nneg Negative terminal
     * @param val Inductance in henrys
     **/
    void makeInductor(HierName name, HierName npos, HierName nneg, double val);

    /**
     * Called by the parser to make a BJT.
     * @param name Name of the diode 
     * @param type Type of the diode
     * @param nc Collection terminal
     * @param nb Base terminal
     * @param ne Emitter terminal
     * @param a Area of the diode in square meters
     **/
    void makeBipolar(HierName name, int type, HierName nc, HierName nb,
                     HierName ne, double a);

    /**
     * Called by the parser to make a call.
     * @param name Name of the instantiation
     * @param subName Name of the subcircuit
     * @param args Arguments to the subcircuit
     * @param parameters Parameters defined on the call (String to Double)
     **/
    void makeCall(HierName name, String subName, HierName[] args, Map parameters);

    /**
     * Called by the parser before processing a subcircuit.
     * @param subName Name of the subcircuit
     * @param in Input nodes
     * @param out Output nodes
     **/
    void beginSubcircuit(String subName, String[] in, String[] out);

    /**
     * Called by the parser after processing a subcircuit.
     * @param subName Name of the subcircuit
     **/
    void endSubcircuit(String subName);
}
