/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.aspice;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Iterator;
import java.io.BufferedWriter;
import java.io.FileWriter;
import com.avlsi.file.common.HierName;
import com.avlsi.util.container.MultiSet;
import com.avlsi.file.spice.SpiceParser;
import com.avlsi.circuit.AbstractCircuit;
import com.avlsi.file.spice.SpiceFileFormatException;

/**
 * Extends NativeAspice to provide namespace management
 * and maintain other state variables that live on the java side.
 * Also performs multi-step functions.
 * This is the correct way to access NativeAspice features.
 *
 * @author Andrew Lines
 * @version $Date$
 **/

public class NativeAspiceUtil extends NativeAspice {
    /** keep trace of currently created circuit handle */
    private int circuitHandle = 0;

    /** has circuit been prepped for simulation? */
    private boolean prepared = false;

    /** basename for files */
    private String basename = null;

    /** current analog time */
    private double analog_time = 0;

    /** analog namespace */
    public MultiSet AnalogNodeNames = null;

    /** options to pass to aspice */
    double timestep = 1e-12;
    double poststep = 10e-12;
    double maxerr = 1e-6;
    double coupling_cutoff = 0.3;
    double trueV = 1.2;
    double falseV = 0;
    double prs_tau = 25e-12;

    /** number of analog devices */
    int Ndevices = 0;

    /** constructor given name of a tracefile */
    public NativeAspiceUtil() {
        System.loadLibrary("aspice");
        circuitHandle = createCircuit();
    }
    
    /** clear everything in the circuit */
    public void clear() {
        System.err.println("clear circuit");
        freeCircuit(circuitHandle);
        circuitHandle = createCircuit();
        Ndevices = 0;
        prepared = false;
        basename = null;
        analog_time = 0;
    }

    /** pass all the options to aspice */
    public void setOptions() {
        setOptions(timestep,poststep,maxerr,coupling_cutoff,trueV,falseV,prs_tau);
    }

    /** simulate circuit for given amount of time */
    public void simulate(double delay) {
        System.err.println("simulate from=" + analog_time + " to=" + (analog_time+delay));
        setOptions();
        if (!prepared) {
            prepareCircuit(circuitHandle);
            summarizeCircuit(circuitHandle);
            // write names file
            try {
                BufferedWriter namesfile = 
                    new BufferedWriter(new FileWriter(basename + ".names"));
                Iterator i = AnalogNodeNames.iterator();
                namesfile.write("time\n");
                while (i.hasNext()) namesfile.write((HierName) i.next() + "\n");
                namesfile.close();
            } catch (IOException e) {
                System.err.println("can't write to names file.");
                return;
            }
            prepared = true;
        }
        simulateCircuit(circuitHandle,basename + ".trace",
                        analog_time,analog_time+delay);
        analog_time += delay;
    }

    /** force an analog node to a given voltage */
    public void forceAnalogNode(HierName name, double voltage) {
        forceAnalogNode(circuitHandle,AnalogNodeToNumber(name),voltage);
    }

    /** handle userinterrupt has occured */
    public void interrupt() {
        System.err.println("interrupted");
    }

    /** select BSIM3 model and temperature to use */
    public void selectModel(String filename, double temperature) {
        setBsim3Model(filename,temperature);
    }

    /** instantiate analog circuit (HACK) */
    public void instantiateSpice(String filename, String cellname) {
        setOptions();
        if      (filename.endsWith(".cdl"))
            basename = filename.substring(0,filename.length()-4);
        else if (filename.endsWith(".spice"))
            basename = filename.substring(0,filename.length()-6);
        else basename = filename;
        prepared = false;

        // first pass of construction -- count devices and create namespace
        SpiceParser parser = new SpiceParser();
        NativeAspiceCircuit.Repository repository = new NativeAspiceCircuit.Repository();
        try {
            parser.parseFile(filename,repository);
        } catch (FileNotFoundException e) {
            System.err.println("can't find " + filename);
            return;
        } catch (IOException e) {
            System.err.println("file trouble " + e);
            return;
        } catch (SpiceFileFormatException e) {
            System.err.println("syntax error " + e);
            return;
        }
        NativeAspiceCircuit circuit = (NativeAspiceCircuit) repository.getCell(cellname);
        AnalogNodeNames = circuit.getNodes();
        
        // allocate memory for analog nodes and devices
        allocateAnalog(circuitHandle,AnalogNodeNames.size(),
                       circuit.getTransistorCount() + circuit.getDiodeCount() + 
                       circuit.getResistorCount() + circuit.getCapacitorCount());
        
        // create analog nodes
        int num = 0;
        Iterator i = AnalogNodeNames.iterator();
        while (i.hasNext()) {
            HierName name = (HierName) i.next();
            createAnalogNode(circuitHandle,num++,1,0,name.toString());
        }

        // second pass of construction -- create devices
        parser = new SpiceParser();
        repository = new NativeAspiceCircuit.Repository();
        repository.adsim = this; // enable second pass
        try {
            parser.parseFile(filename,repository);
        } catch (FileNotFoundException e) {
            System.err.println("can't find " + filename);
            return;
        } catch (IOException e) {
            System.err.println("file trouble " + e);
            return;
        } catch (SpiceFileFormatException e) {
            System.err.println("syntax error " + e);
            return;
        }
    }
    
    /** instantiate prs circuit */
    public void instantiatePrs() {
        System.err.println("instantiate digital");
    }

    /** set various options */
    public void setOption(String type, double value) {
        if      (type.equals("timestep"))        timestep = value;
        else if (type.equals("poststep"))        poststep = value;
        else if (type.equals("maxerr"))          maxerr = value;
        else if (type.equals("coupling_cutoff")) coupling_cutoff = value;
        else if (type.equals("trueV"))           trueV = value;
        else if (type.equals("falseV"))          falseV = value;
        else if (type.equals("prs_tau"))         prs_tau = value;
        else System.err.println("unknown option");
    }

    /** get various options */
    public double getOption(String type) {
        if      (type.equals("timestep"))        return timestep;
        else if (type.equals("poststep"))        return poststep;
        else if (type.equals("maxerr"))          return maxerr;
        else if (type.equals("coupling_cutoff")) return coupling_cutoff;
        else if (type.equals("trueV"))           return trueV;
        else if (type.equals("falseV"))          return falseV;
        else if (type.equals("prs_tau"))         return prs_tau;
        System.err.println("unknown option");
        return 0;
    }

    /** print value of all options */
    public void printOptions() {
        System.out.println("timestep=" + timestep);
        System.out.println("poststep=" + poststep);
        System.out.println("maxerr=" + maxerr);
        System.out.println("coupling_cutoff=" + coupling_cutoff);
        System.out.println("trueV=" + trueV);
        System.out.println("falseV=" + falseV);
        System.out.println("prs_tau=" + prs_tau);
    }

    /** convert analog node name to node number */
    public int AnalogNodeToNumber(HierName name) {
        if (AnalogNodeNames == null) return -1;
        return AnalogNodeNames.findIndex(name);
    }
    
    /** convert digital node name to node number */
    public int DigitalNodeToNumber(HierName name) {
        return -1;
    }

    /** create a transistor from node names */
    public void createTransistor(int type,
                                 HierName S, HierName D, HierName G, HierName B, 
                                 double W, double L) {
        createTransistor(circuitHandle,Ndevices++,(type==1 ? PMOS : NMOS),
                         AnalogNodeToNumber(S), AnalogNodeToNumber(D),
                         AnalogNodeToNumber(G), AnalogNodeToNumber(B),
                         W,L);
    }

    /** create a diode from node names */
    public void createDiode(int type,
                            HierName S, HierName B, 
                            double W, double L, double A, double P) {
        if (W>0.0001) W=0.0001; // HACK
        createDiode(circuitHandle,Ndevices++,(type==1 ? PMOS : NMOS),
                    AnalogNodeToNumber(S), AnalogNodeToNumber(B),
                    W,L,A,P);
    }

    /** create a resistor from node names */
    public void createResistor(HierName S, HierName D, double G) {
        createResistor(circuitHandle,Ndevices++,
                       AnalogNodeToNumber(S), AnalogNodeToNumber(D), 1/G);
    }
    
    /** create a capacitor from node names */
    public void createCapacitor(HierName S, HierName D, double C) {
        createCapacitor(circuitHandle,Ndevices++,
                        AnalogNodeToNumber(S), AnalogNodeToNumber(D), C);
    }
}
