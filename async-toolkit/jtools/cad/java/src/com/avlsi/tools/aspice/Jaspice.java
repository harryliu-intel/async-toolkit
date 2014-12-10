/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.aspice;

import com.avlsi.file.spice.SpiceParser;
import com.avlsi.file.spice.SimulatorInterface;
import com.avlsi.file.spice.SpiceFileFormatException;
import java.util.Properties;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collection;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimSynchronizer;
/**
 * Class for Jaspice
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Jaspice implements SimulatorInterface,DSimSynchronizer.Runnable{
    
    private static Jaspice singleton = null;
    
    //
    //States of the Simulator
    //

    /** Required before starting **/
    private static boolean modelLoaded = false;
    /** Once running cannot change static properties, use reset() to
     * set back to false
     **/
    private boolean running;
   
    /** Repository for all of the cell definitions **/
    private final JaspiceCircuit.Repository repo;
    /** Parser **/
    private final SpiceParser parser;
    /** Analog system's circuit **/
    private JaspiceCircuit circuit=null;
    /** For Output **/
    private JaspiceCallbackInterface callback;

    /** Parameters loaded into the simulator through the spice or the cmdline **/
    private static HashMap params;
    
    /** Current Time (sec)**/
    private double time;
    
    /*****************************************************************
     * Sets the default values to the global parameters
     *****************************************************************/
    public static void setDefaults() {
        params = new HashMap();
        try {
        addProperty("timestep", new JaspiceProp(new Double("1e-12"),
                   "seconds (run)","time of each integration step"));
        addProperty("poststep", new JaspiceProp(new Double("10e-12"),
                   "seconds (run)","time elapsed between each update (ie trace)"));
        addProperty("trace",new JaspiceProp(new Boolean(true),
                   "toggle on/off (run)","turns trace on or off"));
        addProperty("tracename", new JaspiceProp("javatrace",
                   "string (run)",
                   "base name of tracefile (suffix added by simulator)"));
        addProperty("modelfile",new JaspiceProp(
                   "/usr/local/cad/lib/bsim3/tsmc18.bsim3",
                   "string (build)","BSIM3 model filename"));
        addProperty("vdd",new JaspiceProp(
                   "Vdd!",
                   "string (build)","Positive terminal name of default"+
                   " power supply"));
        addProperty("gnd",new JaspiceProp(
                   "GND!",
                   "string (build)","Negative terminal name of default"+
                   " power supply"));
        addProperty("temperature", new JaspiceProp(new Double("27"),
                   "Celsius (build)","temparture of simulation"));
        addProperty("native", new JaspiceProp(new Boolean(true), 
                   "on/off (build)", "turns native models on or off"));
        addProperty("high", new JaspiceProp(new Double("1.8"),
                   "Volts (build)","target Vdd voltage"));
        addProperty("low", new JaspiceProp(new Double("0"),
                   "Volts (build)","target GND voltage"));
        addProperty("timemax", new JaspiceProp(new Double("1e-9"),
                   "seconds (run)",
                   "runtime when run is called with no parameters"));
        addProperty("dsimstep",new JaspiceProp(new Integer("0"),
                   "dsim cycles (run)",
                   "number of dsim cycles to take with each timestep"));
        addProperty("prstau", new JaspiceProp(new Double("25e-12"),
                   "seconds (run)",
                   "paramter for digital to analog conversion"));
        addProperty("maxerr", new JaspiceProp(new Double("1e-6"),
                   " (run)",
                   "relaxation parameter"));
        addProperty("transconductance", new JaspiceProp(new Double("1"),
                   "mhos (build (build))",
                   "Voltage Source Parameter"));
        } catch (PropertyException e) {}
    }

    /** return the current simulation time **/
    public double getTime() { return time;}

    /** Load the model vars for the simulatio, uses properties modelfile
     * and temperature
     **/
    private void loadModel() 
            throws PropertyException {
        String modelfile = getStringProperty("modelfile");
        double temp = getDoubleProperty("temperature");
        boolean useNative = getBooleanProperty("native");
        BSim3Model.readFile(modelfile,temp,useNative);
        modelLoaded = true;
    }
   
    /** Returns the Jaspice object **/
    public static Jaspice get() {
        if ((singleton==null) ||
            (!modelLoaded)){ 
            singleton = new Jaspice(); 
        }
        return singleton;
    }
    
    /**
     * Secret Constructor.
     **/
    private Jaspice() {
        circuit = new JaspiceCircuit("");
        params = new HashMap();
        setDefaults();
        parser = new SpiceParser('.', null, null, this);
        repo = new JaspiceCircuit.Repository();
        callback = null;
        running = false;
        time = 0f;
        Runtime.getRuntime().addShutdownHook(
                new Thread(
                    new Runnable() {
                        public void run() {
                            cleanup();
                        }
                    }));
    }
    
    /** Sets the callback to call update on foreach timestep **/
    public void setCallback(JaspiceCallbackInterface callback) {
        this.callback = callback;
    }
    
    /** Returns the node with name <code>name</code>**/
    public Node findNode(String name) {
        if (circuit != null) return circuit.findNode(name);
        return null;
    }
    
    /** returns the device in the circuit **/
    public AbstractDevice findDevice(String name) {
        if (circuit != null) return circuit.findDevice(name);
        return null;
    }

    /** Gets all nodes in the circuit 
     * @return A collection of Nodes**/
    public Collection getNodes() {
        if (getCircuit() == null) return null;
        return getCircuit().getNodes();
    }
    
    /** Gets all devices in the circuit
     * @return A collection of AbstractDevices **/
    public Collection getDevices() {
        if (getCircuit() == null) return null;
        return getCircuit().getDevices();
    }

    /** Build a circuit within the repository with the given parameters 
     * @param filename File that holds the celltype
     * @param cellname Name of the cell to instantiate
     * @param fullname Prefix appended to the nodenames, set to "" when
     *  not in use
     ***/
    public void buildCircuit(final String filename, 
                             final String cellname,
                             final String fullname) 
            throws JaspiceException {
        double high=0,low=0,maxerr=0;
        String vdd,gnd;
        try {
            high = getDoubleProperty("high");
            low = getDoubleProperty("low");
            maxerr = getDoubleProperty("maxerr");
            vdd = getStringProperty("vdd");
            gnd = getStringProperty("gnd");
        } catch (PropertyException e) {
            throw new JaspiceException(e);
        }
        HierName hvdd=null,hgnd=null;
        try {
            hvdd = HierName.makeHierName(vdd,'.');
            hgnd = HierName.makeHierName(gnd,'.');
        } catch (InvalidHierNameException e) {
            System.out.println(e.getMessage());
        }
        JaspiceCircuit.groundName = hgnd;
        /** Loads the model **/
        try {
            loadModel();
        } catch (PropertyException e) {
            System.out.println("Invalid Property, model not loaded: "+
                               e.getMessage());
        }
        /** Builds the specific instance **/
        try {
            HierName hname = HierName.makeHierName(fullname);
        } catch (IllegalArgumentException e) {
            System.out.println("Error in hierarchical fullname: "+
                                e.getMessage());
        }
        /** Retrieve the cell definition **/
        JaspiceCircuit celldef = (JaspiceCircuit) repo.getCell(cellname);
        if (celldef == null) {
            try {
                parser.parseFile(filename, repo);
            } catch (SpiceFileFormatException e) {
                throw new JaspiceException("Parse Error: "+e.getMessage(), e);
            } catch (java.io.IOException e) {   
                throw new JaspiceException("Parse Error: "+e.getMessage(), e);
            }
            /*for (Iterator i = repo.getCellTypesIterator(); i.hasNext();) {
                System.out.println((String) i.next());
            }*/
            celldef =(JaspiceCircuit) repo.getCell(cellname);
            if (celldef == null)
                throw new JaspiceException("Cell definition "+cellname+
                                           " not found in "+filename);
        }

        //JaspiceCircuit jcircuit = (JaspiceCircuit) deepCopy(celldef);
        /** Add the instance to the instance space **/
        //circuits.put(fullname, jcircuit);
        //circuit.addSubcell(fullname, jcircuit);
        //FIX ME? flattens the cells in the repo as well!
        circuit.addSubcell(fullname, celldef);
        circuit.flatten();
        circuit.setHigh(high);
        circuit.setLow(low);
        circuit.setMaxerr(maxerr);
        if (!circuit.powered())
            try {
                circuit.addPowerSupply(hvdd, hgnd);
                System.out.println("Power Supply added across "+vdd+" and "+gnd);
            } catch(JaspiceCircuit.JaspiceCircuitException e) {
                System.out.println("Warning : "+e.getMessage());
            }
        circuit.printStats(); 
    }

    /** Tests to see if the simulation has been initialized and started **/
    public boolean isRunning() { return running; }

    /** Initialize on first run **/
    private void initialize(double timestep) 
            throws JaspiceException {
        if (callback != null)
            callback.init();
        if (circuit == null)
            throw new JaspiceException("No circuit built, cannot initialize.");
        circuit.setTimeStep(timestep);
        Jaspice.setDSimTime(DSim.get().getTime());
    }
        
    /** Run the simulation
     * @param newDur Duration to run the simulation, a newDur of zero will
     * run the simulation to timemax **/
    public void run(double newDur) 
            throws JaspiceException, PropertyException {
        if (circuit == null) throw new JaspiceException("No circuit built.");
        if (callback == null) System.out.println("WARNING: No callback used.");
        double timemax = newDur;
        if (newDur == 0)
             timemax = getDoubleProperty("timemax");
        double timestep = getDoubleProperty("timestep");
        double poststep = getDoubleProperty("poststep");
        int dsimstep = getIntegerProperty("dsimstep");
        double prstau = getDoubleProperty("prstau");
        if (!running) { //initialize
            initialize(timestep);
            running = true;
        }
        long numsteps = Math.round((timemax - time)/timestep);
        double starttime = time;
        long totalsteps = numsteps;
        long iterations = 0;
        while (numsteps-- > 0 ) {

            iterations += circuit.step();
            JaspiceCircuit.digitalStep(dsimstep);
            if (callback != null) callback.update();
            time+= timestep;
        }
        System.out.println("Ran from "+starttime+" to "+time+" ("+
                           totalsteps+" steps, iterations/step= "+
                           (iterations/totalsteps)+")");
    }

    /** Close stuff on exit **/
    private void cleanup() {
        try {
            if (callback != null) 
                callback.finish();
        } catch (JaspiceException e) {
            System.out.println("Error, callback couldn't finish:\n\t"+e.getMessage());
        }
    }
    /** Set the dsim time internally in Jaspice.  Because Jaspice is timesteped,
     * and DSim only cycles when it has inputs, Jaspice needs to keep an 
     * internal count on the amount it needs to stroke DSim to stay synchronized
     * **/
    public static void setDSimTime(long dtime) {
        JaspiceCircuit.setDSimTime(dtime);
    }

    /** Get a property from the Jaspice parameter list 
     * @return A JaspiceProp with the proper parameter (or null if not found)**/
    private JaspiceProp getProperty(String key)
            throws PropertyException {
        if (!params.containsKey(key))
            throw new PropertyException("Property with key "+key+
                                                " not found.");
        return (JaspiceProp) params.get(key);
    }

    /** Return a formatted string for use in printing to stdout or to
     * a file **/

    public String printProperty(String key)
            throws PropertyException {
        return getProperty(key).printProp(key);
    }

    /** Get all of the names of the properties in the parameter list
     * @return A collection of strings**/
    public Collection getPropertyKeys() {
        return params.keySet();
    }

    public static void addProperty(String key, JaspiceProp jprop) 
            throws PropertyException {
        if (params == null) params = new HashMap();
        if (params.containsKey(key))
            throw new PropertyException("Key "+key+" already exists, cannot"+
                                        " add again.");
        params.put(key,jprop);
    }
    /****************************************************************
     * Methods to parse properties into different kinds of base types
     ****************************************************************/

    /** Get the property only if it is a double, throw an exception otherwise **/
    public double getDoubleProperty(String key) 
            throws PropertyException{
        JaspiceProp jp = (JaspiceProp) params.get(key);
        if (jp == null)
            throw new PropertyException("Key "+key+" not found.");
        Object o = jp.getValue();
        if (!(o instanceof Double))
            throw new PropertyException(key+"'s value is not a Double");
        return ((Double) o).doubleValue();
    }

    /** Get the property only if it is an integer, throw an exception otherwise **/
    public int getIntegerProperty(String key) 
            throws PropertyException{
        JaspiceProp jp = (JaspiceProp) params.get(key);
        if (jp == null)
            throw new PropertyException("Key "+key+" not found.");
        Object o = jp.getValue();
        if (!(o instanceof Integer))
            throw new PropertyException(key+"'s value is not an Integer");
        return ((Integer) o).intValue();
    }

    /** Get the property only if it is a boolean, throw an exception otherwise **/
    public boolean getBooleanProperty(String key)
            throws PropertyException{
        JaspiceProp jp = (JaspiceProp) params.get(key);
        if (jp == null)
            throw new PropertyException("Key "+key+" not found.");
        Object o = jp.getValue();
        if (!(o instanceof Boolean))
            throw new PropertyException(key+"'s value is not a Boolean");
        if (((Boolean) o).equals(Boolean.TRUE)) return true;
        return false;
    }
    
    /** Get the property only if it is a string, throw an exception otherwise **/
    public String getStringProperty(String key) 
            throws PropertyException{
        JaspiceProp jp = (JaspiceProp) params.get(key);
        if (jp == null)
            throw new PropertyException("Key "+key+" not found.");
        Object o = jp.getValue();
        if (!(o instanceof String))
            throw new PropertyException(key+"'s value is not a String");
        return (String) o;       
    }
   
    /*****************************************************************
     * Classes to add different types of drivers to the nodes
     *****************************************************************/

    /** Drive the simulation with a digital node 
     * @param aname Name of the analog node to drive
     * @param dname Name of the digital node driving**/
    public void addDigitalDriver(String aname, String dname) 
            throws JaspiceException {
        Node node = findNode(aname);
        com.avlsi.tools.dsim.Node dnode = DSim.get().findNode(dname);
        if (node != null) {
            if (dnode != null) {
                try {
                    double prstau = getDoubleProperty("prstau");
                    DigitalDriver dd = new DigitalDriver(prstau);
                    node.setDriver(dd);
                    dnode.addWatch(dd);
                } catch (PropertyException e) {
                    throw new JaspiceException(e);
                }
                System.out.println("Analog node "+node.getName()+
                                   " will be driven by "+
                                   dnode.getName());
            } else throw new JaspiceException("Cannot find digital node "+
                                              dname);
        } else throw new JaspiceException("Node "+aname+" not found.");
    }

    /** Adds a source handler to the circuit **/
    public void addSourceHandler(String key, SourceHandler sH) {
        JaspiceCircuit.addSourceHandler(key,sH);
    }

    /** adds a source to the circuit **/
    public void addSource(String name, String type, String pname, String nname,
                          String[] args) throws JaspiceException {
       if (circuit == null)
           throw new JaspiceException("No circuit built, cannot add source.");
       try {
            HierName hp = HierName.makeHierName(pname, '.');
            HierName hn = HierName.makeHierName(nname, '.');
            HierName hname = HierName.makeHierName(name, '.');
           //Use the Source just to make passing easier
            circuit.addSource(new com.avlsi.circuit.Source(
                             hname, type, hp, hn, args));
       } catch(InvalidHierNameException e) {
           throw new JaspiceException(e);
       } catch (JaspiceCircuit.Exception e) {
           throw new JaspiceException(e);
       }
    }   
    
    /*****************************************************************
     * Class to represent exceptions in Property processing.
     *****************************************************************/
    public static final class PropertyException
            extends java.lang.Exception {
        public PropertyException(final String message) {
            super(message);
        }
    }
    
    /*****************************************************************
     * Set a static variable
     *****************************************************************/
    public static void set(String key, String val) 
        throws PropertyException {
            if (!params.containsKey(key))
                throw new PropertyException("Key "+key+" not found");
            JaspiceProp jp = (JaspiceProp) params.get(key);
            //Supports doubles, ints and booleans, the rest go as strings
            Object o = jp.getValue();
            if (o instanceof Double) {
                jp.setValue(new Double(val));
            } else if (o instanceof Integer) {
                jp.setValue(new Integer(val));
            } else if (o instanceof Boolean) {
                if ((val.equalsIgnoreCase("on")) ||
                    (val.equals("1"))) val = "true";
                if ((val.equalsIgnoreCase("off")) ||
                    (val.equals("0"))) val = "false";
                jp.setValue(new Boolean(val));
            } else jp.setValue(val);
    }

    /** Get the toplevel circuit modeled in Jaspice **/
    private JaspiceCircuit getCircuit() {
        return circuit;
    }

    /** Add alias <code>name</code> to node <code>node</code>.  
     * Assumes a flattened circuit**/
    public void aliasNode(Node node, String name) 
            throws JaspiceException {
        Node node2 = findNode(name);
        if (node2 != null) {
            throw new JaspiceException("Node with name "+name+
            " already exists, cannot alias two nodes that"+
            " already exist.");
        }
        try {
            circuit.aliasNames(node.getName(),
                               HierName.makeHierName(name,'.'));
        } catch (InvalidHierNameException e) {
            throw new JaspiceException("The name "+name+
                                     " is not a valid HierName.", e);
        }
    }

    /** Returns an iterator of HierNames containing all aliases.  One
     * alias will be a pointer (name.isPointer() == true) **/
    public Iterator getAliases(Node node) {
        
        return circuit.getAliases(node);
    }

    //
    //Simulator Interface functions
    //
    
    //private boolean firstprint = true;
    /** Not implemented currently **/
    public void printStatement(String type, String args[]) 
            throws SpiceFileFormatException {
/*        if (!type.equals("TRAN")) 
            throw new SpiceFileFormatException("Only TRAN (transient) .print"+
                                               " functions supported.");
        if (circuit == null)
            throw new SpiceFileFormatException("No circuits built, .print ignored");
        if (firstprint) {
            try {
                clearTraceList();
            } catch (JaspiceException e) {
                throw new SpiceFileFormatException("Error in simulator .PRINT"+
                                                   " : "+e.getMessage());
            }
            firstprint = false;
        }
        for(int i=0;i<args.length;i++) {
            try {
                traceNode(args[i]);
            } catch(JaspiceException e) {
                throw new SpiceFileFormatException(e.getMessage());
            }
        }*/
    }

    /** Support for .OPTION **/
    public void setStatement(String key, String value)
            throws SpiceFileFormatException{
        try {
            set(key.toLowerCase(),value);
            System.out.println("Set "+key.toLowerCase()+" to "+value);
           } catch (PropertyException e) {
               throw new SpiceFileFormatException(e);
           }
    }
}

