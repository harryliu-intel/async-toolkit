/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.aspice;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.HashSet;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.util.container.MultiSet;

import com.avlsi.util.recalc.IndependantVariable;
import com.avlsi.util.recalc.MinusOp;
import com.avlsi.util.recalc.PlusOp;
import com.avlsi.util.recalc.DivOp;
import com.avlsi.util.recalc.MultOp;
import com.avlsi.util.recalc.Literal;

import com.avlsi.circuit.AbstractCircuit;
import com.avlsi.circuit.ResistorInterface;
import com.avlsi.circuit.DiodeInterface;
import com.avlsi.circuit.TransistorInterface;
import com.avlsi.circuit.CapacitorInterface;
import com.avlsi.circuit.SourceInterface;

import com.avlsi.tools.dsim.DSim;

/** NativeAspice Circuit needed for parsing */
public class NativeAspiceCircuit extends AbstractCircuit {

    /** control verbosity */
    private boolean verbose = false;

    /** Which pass are we on? */
    private boolean firstpass = false;

    /** HierName-to-Node map for the entire circuit. */
    private MultiSet nodes;

    /** Which adsim to create stuff in */
    private NativeAspiceUtil adsim = null;

    /** counts of various devices */
    private int numVSources=0;
    private int numISources=0;
    private int numResistors=0;
    private int numCapacitors=0;
    private int numTransistors=0;
    private int numDiodes=0;

    /** methods to return count of various devices */
    public int getResistorCount()   { return numResistors; }
    public int getCapacitorCount()  { return numCapacitors; }
    public int getTransistorCount() { return numTransistors; }
    public int getDiodeCount()      { return numDiodes; }
    public int getNodeCount()       { return nodes.size(); }
    public int getVSourceCount()    { return numVSources; }
    public int getISourceCount()    { return numISources; }

    /** methods to instantiate things */
    public void addResistor(ResistorInterface r) {
        if (firstpass) {
            if (verbose) 
                System.err.println("add resistor drain=" + r.getDrain() + 
                                   " source=" + r.getSource() + 
                                   " G=" + r.getConductance());
            nodes.addIfUnique(r.getDrain());
            nodes.addIfUnique(r.getSource());
        } else {
            adsim.createResistor(r.getSource(),r.getDrain(),
                                 r.getConductance());
        } 
        numResistors++;
    }
    public void addCapacitor(CapacitorInterface c) {
        if (firstpass) {
            if (verbose)
                System.err.println("add capacitor drain=" + c.getDrain() + 
                                   " source=" + c.getSource() + 
                                   " C=" + c.getCapacitance());
            nodes.addIfUnique(c.getDrain());
            nodes.addIfUnique(c.getSource());
        } else {
            adsim.createCapacitor(c.getSource(),c.getDrain(),
                                  c.getCapacitance());
        }
        numCapacitors++;
    }
    public void addTransistor(TransistorInterface t) {
        if (firstpass) {
            if (verbose) 
                System.err.println("add transistor source=" + t.getSource() + 
                                   " drain=" + t.getDrain() + 
                                   " gate=" + t.getGate() +
                                   " bulk=" + t.getBulk() + 
                                   " W=" + t.getWidth() + 
                                   " L=" + t.getLength() + 
                                   " type=" + t.getType());
            nodes.addIfUnique(t.getDrain());
            nodes.addIfUnique(t.getSource());
            nodes.addIfUnique(t.getBulk());
            nodes.addIfUnique(t.getGate());
        } else {
            adsim.createTransistor(t.getType(),
                                   t.getSource(),t.getDrain(),t.getGate(),t.getBulk(),
                                   t.getWidth(),t.getLength());
        }    
        numTransistors++;
    }
    public void addDiode(DiodeInterface diode) {
        if (firstpass) {
            if (verbose) 
                System.err.println("add diode source=" + diode.getSource() +
                                   " drain=" + diode.getDrain() + 
                                   " area=" + diode.getArea() +
                                   " width=" + diode.getWidth() +
                                   " length=" + diode.getLength() + 
                                   " perim=" + diode.getPerimeter() + 
                                   " type=" + diode.getType());
            nodes.addIfUnique(diode.getDrain());
            nodes.addIfUnique(diode.getSource());
        } else {
            adsim.createDiode(diode.getType(),
                              diode.getSource(),diode.getDrain(),
                              diode.getWidth(),diode.getLength(),
                              diode.getArea(),diode.getPerimeter());
        }
        numDiodes++;
    }
    public void addSource(SourceInterface source) {
        System.err.println("Sources not supported");
    }

    /** first pass constructor */
    public NativeAspiceCircuit(String cellType, NativeAspiceUtil adsim) {
        super(cellType);
        if (adsim==null) {
            firstpass = true;
            nodes = new MultiSet();
            this.adsim = null;
        }
        else {
            firstpass = false;
            nodes = adsim.AnalogNodeNames;
            this.adsim = adsim;
        }
    }

    /** return nodes */
    public MultiSet getNodes() {
        nodes.sort();
        return nodes;
    }

    /** Static subcircuit repository nested subclass. */
    public static class Repository extends AbstractCircuit.Repository {
        public NativeAspiceUtil adsim = null; // null on first pass
        
        /** CircuitGraph generator method. */
        protected AbstractCircuit newCircuitGenerator(final String cellType) {
            return new NativeAspiceCircuit(cellType,adsim);
        }
    }
}
