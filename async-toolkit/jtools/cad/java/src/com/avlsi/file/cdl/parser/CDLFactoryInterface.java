/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.util.Map;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;

/**
 * Factory interface used by the CDL parser to create devices.
 **/
public interface CDLFactoryInterface {
    /**
     * Called by the parser to make a resistor.
     * @param name Name of the resistor
     * @param n1 First terminal
     * @param n2 Second terminal
     * @param val Resistance in ohm
     * @param parameters Parameters specified for this resistor
     * @param env Current environment
     **/
    void makeResistor(HierName name, HierName n1, HierName n2,
                      CDLLexer.InfoToken val, Map parameters, Environment env);

    /**
     * Called by the parser to make a capacitor.
     * @param name Name of the capacitor
     * @param npos Positive terminal
     * @param nneg Negative terminal
     * @param val Capacitance in farads
     * @param parameters Parameters specified for this capacitor
     * @param env Current environment
     **/
    void makeCapacitor(HierName name, HierName npos, HierName nneg,
                       CDLLexer.InfoToken val, Map parameters, Environment env);

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
     * @param parameters Parameters specified for this transistor
     * @param env Current environment
     **/
    void makeTransistor(HierName name, String type, HierName ns, HierName nd,
                        HierName ng, HierName nb, CDLLexer.InfoToken w,
                        CDLLexer.InfoToken l, Map parameters, Environment env);

    /**
     * Called by the parser to make a diode.
     * @param name Name of the diode 
     * @param type Type of the diode
     * @param npos Positive terminal
     * @param nneg Negative terminal
     * @param area Area of the diode in square meters
     * @param parameters Parameters specified for this diode
     * @param env Current environment
     **/
    void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                   CDLLexer.InfoToken area,  Map parameters, Environment env);

    /**
     * Called by the parser to make an inductor.
     * @param name Name of the inductor 
     * @param npos Positive terminal
     * @param nneg Negative terminal
     * @param val Inductance in henrys
     * @param parameters Parameters specified for this inductor
     * @param env Current environment
     **/
    void makeInductor(HierName name, HierName npos, HierName nneg,
                      CDLLexer.InfoToken val, Map parameters, Environment env);

    /**
     * Called by the parser to make a BJT.
     * @param name Name of the diode 
     * @param type Type of the diode
     * @param nc Collector terminal
     * @param nb Base terminal
     * @param ne Emitter terminal
     * @param area Area of the diode in square meters
     * @param parameters Parameters specified for this diode
     * @param env Current environment
     **/
    void makeBipolar(HierName name, String type, HierName nc, HierName nb,
                     HierName ne, CDLLexer.InfoToken area,  Map parameters,
                     Environment env);


    /**
     * Called by the parser to make a call.
     * @param name Name of the instantiation
     * @param subName Name of the subcircuit
     * @param args Arguments to the subcircuit
     * @param parameters Overriding parameters specified for this call
     * @param env Current environment
     **/
    void makeCall(HierName name, String subName, HierName[] args,
                  Map parameters, Environment env);

    /**
     * Called by the parser before processing a subcircuit.
     * @param subName Name of the subcircuit
     * @param in Input nodes
     * @param out Output nodes
     * @param parameters Default parameters for this subcircuit
     * @param env Current environment
     **/
    void beginSubcircuit(String subName, String[] in, String[] out,
                         Map parameters, Environment env);

    /**
     * Called by the parser after processing a subcircuit.
     * @param subName Name of the subcircuit
     * @param env Current environment
     **/
    void endSubcircuit(String subName, Environment env);
}
