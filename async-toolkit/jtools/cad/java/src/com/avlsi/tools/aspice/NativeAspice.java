/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.aspice;

/**
 * JNI wrapper for native C aspice and dsim.
 * C pointer to CIRCUIT is represented as an int.
 * All devices and nodes are referred to by number.
 *
 * @author Andrew Lines
 * @version $Date$
 **/

public class NativeAspice {

    /** Define transistor constants */
    public static final int NMOS = +1;
    public static final int PMOS = -1;

    /** Set current bsim3 model */
    static native void setBsim3Model
        (String filename, double temperature);
    
    /** Set simulation options */
    static native void setOptions
        (double timestep, double poststep,
         double maxerr, double coupling_cutoff,
         double trueV, double falseV, double prs_tau);
    
    /** Create an empty circuit */
    static native int createCircuit();
    
    /** Allocate memory for analog nodes and devices */
    static native void allocateAnalog
        (int circuitHandle, int Nnodes, int Ndevices);
    
    /** Allocate memory for digital nodes and rules */
    static native void allocateDigital
        (int circuitHandle, int Ndnodes, int Ndrules);

    /** Link analog to digital node */
    static native void linkAnalogDigitalNode
        (int inode, int idnode);
    
    /** Free memory associated with a circuit */
    static native void freeCircuit
        (int circuitHandle);
    
    /** Prepare a circuit for simulation */
    static native void prepareCircuit
        (int circuitHandle);

    /** Summarize statistics of a circuit */
    static native void summarizeCircuit
        (int circuitHandle);

    /** Simulate the circuit with current options */
    static native void simulateCircuit
        (int circuitHandle, String tracefilename, double from, double to);
    
    /** Create a digital node */
    static native void createDigitalNode
        (int circuitHandle, int idnode, String name);

    /** Create a digital production rule */
    static native void createDigitalRule
        (int circuitHandle, int idrule,
         int Nguards, int [] iguards, int [] sense,
         int itarget, int dir, int delay);

    /** Create an analog node */
    static native void createAnalogNode
        (int circuitHandle, int inode, int watch, int warn, String name);
  
    /** Force an analog node to given voltage */
    static native void forceAnalogNode
        (int circuitHandle, int inode, double forceV);
    
    /** Create a capacitor */
    static native void createCapacitor
        (int circuitHandle, int idev, int iA, int iB, double C);

    /** Create a resistor */
    static native void createResistor
        (int circuitHandle, int idev, int iA, int iB, double R);
    
    /** Create a BSIM3 source/drain resistor */
    static native void createSourceResistor
        (int circuitHandle, int idev, int type, int source, 
         int iA, int iB, double W, double L, double NRS, double RSC);

    /** Create a BSIM3 diode */
    static native void createDiode
        (int circuitHandle, int idev, int type,
         int iS, int iB, double W, double L, double AS, double PS);
    
    /** Create a BSIM3 transistor */
    static native void createTransistor
        (int circuitHandle, int idev, int type,
         int iS, int iD, int iG, int iB, double W, double L);

    /** constructor */
    NativeAspice() {
    }
    
    /** self-test 3 inverter ring oscillator */
    public static void main(String [] args) {
        // load c libraries
        System.err.println("Loading c library...");
        System.loadLibrary("aspice");

        // set simulation options
        System.err.println("Setting simulation options...");
        setOptions(1e-12,10e-12,1e-6,0.3,1.2,0,25e-12);

        // set default BSIM3 model
        System.err.println("Setting default BSIM3 model...");
        setBsim3Model("tsmc18.bsim3",25);

        // create circuit
        System.err.println("Creating circuit...");
        int circuit = createCircuit();
        allocateAnalog(circuit,5,6);

        // create nodes
        System.err.println("Creating nodes...");
        int GND=0, Vdd=1;
        int [] x = {2,3,4};
        createAnalogNode(circuit,0,1,0,"GND");
        createAnalogNode(circuit,1,1,0,"Vdd");
        createAnalogNode(circuit,2,1,0,"x[0]");
        createAnalogNode(circuit,3,1,0,"x[1]");
        createAnalogNode(circuit,4,1,0,"x[2]");

        // create devices
        System.err.println("Creating devices...");
        for (int i=0; i<3; i++) {
            createTransistor(circuit,2*i+0,NMOS,GND,x[(i+1)%3],x[i],GND,0.6e-6,0.18e-6);
            createTransistor(circuit,2*i+1,PMOS,Vdd,x[(i+1)%3],x[i],Vdd,0.9e-6,0.18e-6);
        }

        // prepare the circuit
        System.err.println("Prepare and summarize circuit...");
        prepareCircuit(circuit);
        summarizeCircuit(circuit);

        // simulate
        System.err.println("Simulating circuit...");
        forceAnalogNode(circuit,0,0);
        forceAnalogNode(circuit,1,1.8);
        simulateCircuit(circuit,"ring.trace",0,10e-9);

        // finish
        System.err.println("Done.");
    }
}

