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

/***********************************************
 * Jaspice Circuit, the implementation of the Jaspice Model
 ***********************************************/
public class JaspiceCircuit extends AbstractCircuit { 

    /** All nodes with this name will become GroundNodes, instead of nodes **/
    public static HierName groundName = HierName.makeHierName("GND!");

    /** List of AbstractDevice objects in the circuit. **/
    private HashMap devices;

    /** Weighting constant for charge data. **/
    private double chargeScale;

    /** Weighting constant for current data. **/
    private double currentScale;

    /** Weighting constant for charge derivative data. **/
    private double derivChargeScale;

    /** Weighting constant for current derivative data. **/
    private double derivCurrentScale;

    /** Threshold for relaxation voltage change. **/
    private double relaxationThreshold = 1e-6;

    /** Amount of time lapsed in one timestep. **/
    private double timestep;

    /** Amount of DSim time elapsed (need to keep local copy) **/
    private static long dsimtime=0;
    
    /** Current simulation time. **/
    private double time;

    /***********************************************
     * Number of Voltage Sources
     ***********************************************/

    private int numVSources=0;
    
    /***********************************************
     * Number of Current Sources
     ***********************************************/

    private int numISources=0;
    
    /***********************************************
     * The number of resistors in the circuit.
     ***********************************************/
    private int numResistors=0;

    /***********************************************
     * The number of capacitors in the circuit.
     ***********************************************/
    private int numCapacitors=0;

    /***********************************************
     * The number of transistors in the circuit.
     ***********************************************/
    private int numTransistors=0;

    /***********************************************
     * The number of transistors in the circuit.
     ***********************************************/
    private int numDiodes=0;

    /***********************************************
     * HierName-to-Node map for the entire circuit.
     ***********************************************/
    private HashMap nodeMap;

    /***********************************************
     * HashMap of all possible source types
     ***********************************************/
    private static HashMap sourceTypes = 
        SourceHandler.getDefaultTypes();
    /***********************************************
     * Circuit parameters that can be set to the
     * simulation's liking
     ***********************************************/

    /** Target Vdd Voltage **/
    private double high;  
    /** Target GND voltage **/
    private double low;
    /** Does this circuit have power? **/
    private boolean power;
    
    public void setHigh(double h) {
        this.high = h;
    }

    public void setLow(double l) {
        this.low = l;
    }

    /** Set the relaxation threadhold **/
    public void setMaxerr(double rt) {
        relaxationThreshold = rt;
    }

    public void setTimeStep(double timestep) {
        this.timestep = timestep;
        this.chargeScale = 0;
        this.currentScale = timestep;
        this.derivChargeScale = 1;
        this.derivCurrentScale = -timestep/2;
        for (Iterator i = getSubcellNames();i.hasNext();) {
            ((JaspiceCircuit)getSubcellDefForName((String)i.next()))
            .setTimeStep(timestep);
        }
    }

    /***********************************************
     * Uninitialized CircuitGraph constructor.
     ***********************************************/
    public JaspiceCircuit(String cellType) {
        super(cellType);
        nodeMap = new HashMap();
        devices = new HashMap();
        setTimeStep(1e-12); //default timestep
        power = false; //no power supply
    }

    /** Does the circuit have an attached power supply? **/
    public boolean powered() { return power; }
    /** Add a power supply across nodes with names vddname and groundname **/
    public void addPowerSupply(HierName vddname, HierName gndname) 
            throws JaspiceCircuitException {
        // Cheat on the power supply
        Node vdd = (Node) nodeMap.get(vddname);
        if (vdd == null) throw new JaspiceCircuitException("Node named "+vddname+
                                                    " not found, power supply"+
                                                    " not connected.");
        Node ground = (Node) nodeMap.get(gndname);
        if (ground== null) throw new JaspiceCircuitException("Node named "+gndname+
                                                    " not found, power supply"+
                                                    " not connected.");
        IndependantVariable x = new IndependantVariable(0);
        MultOp exp = new MultOp(new MinusOp(new Literal(high), x),
                                 new Literal(1e10));
        HashMap voltageMap = new HashMap();
        voltageMap.put(vdd, x);
        addDevice(
                new com.avlsi.tools.aspice.CurrentSource(
                    HierName.makeHierName("PowerSupply"),
                    vdd, exp, voltageMap));
        addDevice(
               new com.avlsi.tools.aspice.Capacitor(vdd, ground, 1));
        numISources++;
        numCapacitors++;
        power = true;
    }

    private int lastId = 0;
    /**
    * Add device to circuit.
    * @param device to add
    **/
    public void addDevice( AbstractDevice device) {
        HierName hname = device.getName();
        if ((hname == null) ||
            devices.containsKey(hname)) {
            //Generate a random device name
            hname = HierName.makeHierName(device.getCode()+"t_e_m_p"+(lastId++));
            device.setName(hname);
            addDevice(device);
        } else
            devices.put(hname,device);
    }

    /** Add the node to the circuit hierarchy **/
    public Node addNode(HierName h,boolean isGround) {
        Node node = null;
        if (isGround)
             node = new GroundNode(h);
        else node = new Node(h);
        node.setHash(h);
        nodeMap.put(node.getHash(),node);
        //Add the name to the alias map
        addName(h);
        return node;
    }
    
    /** Adds a node to the nodeMap if it doesn't already exist **/
    private Node processNode(HierName h) {
        Node node = (Node)nodeMap.get(h);
        if (node == null) 
            node = addNode(h,h.equals(groundName));
        HierName ptrname = HierName.makeSiblingName(h, h.getSuffixString()+"#");
        aliasNames(h, ptrname);
        return node;
    }

    /** Returns the internal time **/
    public double getTime() { return time; }
    
    /** Get the node aliases from the alias map, names are HeirNames **/
    public Iterator getAliases(Node node) {
        return getEquivalentNames(node.getName());
    }
    
    /*****************************************************************
     * Add a resistor to the circuit
     *****************************************************************/
    public void addResistor(ResistorInterface r) {
        Node d = processNode(r.getDrain());
        Node s = processNode(r.getSource());
        addDevice( new Resistor(r.getName(), s,d, r.getConductance()));
        numResistors++;
    }

    /*****************************************************************
     * Add a capacitor to the circuit
     *****************************************************************/
    public void addCapacitor(CapacitorInterface c) {
        Node d = processNode(c.getDrain());
        Node s = processNode(c.getSource());
        addDevice( new Capacitor(c.getName(),s,d, c.getCapacitance()));
        numCapacitors++;
    }

    /*****************************************************************
     * Add a (MOSFET) transistor to the circuit
     *****************************************************************/
    public void addTransistor(TransistorInterface t)
    {
        Node d = processNode(t.getDrain());
        Node s = processNode(t.getSource());
        Node g = processNode(t.getGate());
        Node b = processNode(t.getBulk());

        addDevice(
            new Transistor(t.getName(),s,d,g,b,t.getType(),
                           t.getWidth(),t.getLength()));
        numTransistors++;
    }

    /*****************************************************************
     * Add a diode to the circuit.
     *****************************************************************/
    public void addDiode(DiodeInterface diode) {
        /** It seems that the SpiceParser produces diodes connected
         * to the same node on both ends.  Ignore these (useless) 
         * devices
         **/
        if (diode.getSource().equals(diode.getDrain())) return;
        Node s = processNode(diode.getSource());
        Node d = processNode(diode.getDrain());

        addDevice(
                new Diode(diode.getName(),
                          diode.getType(),
                          s,d,
                          diode.getWidth(), diode.getLength(),
                          diode.getArea(), diode.getPerimeter()));
        numDiodes++;
    }

    public static void addSourceHandler(String type, SourceHandler sH) {
        sourceTypes.put(type, sH);
    }

    /*****************************************************************
     * Add a source to the circuit.
     *****************************************************************/
    public void addSource(HierName name, String type, Node nplus, Node nminus,
                          String[] args) throws AbstractCircuit.Exception {
        SourceHandler handler = 
            (SourceHandler) sourceTypes.get(type.toUpperCase());
        if (handler == null)
            throw new AbstractCircuit.Exception("Voltage source type "+
                                                  type+" unknown.");
        try {
            addDevice( handler.buildSource(name, nplus, nminus, args));
        } catch(SourceHandler.SourceHandlerException e) {
            throw new AbstractCircuit.Exception(e);
        }
        numVSources++;
    }
        
    /*****************************************************************
     * Adds a source to the circuit.
     *****************************************************************/
    public void addSource(SourceInterface source) 
            throws AbstractCircuit.Exception {

        Node pnode = processNode(source.getPositiveTerminal());
        Node nnode = processNode(source.getNegativeTerminal());
        addSource(source.getName(),source.getType(),
                  pnode,nnode,source.getArguments());
   
    }                                
    
    /*****************************************************************
     * Returns the number of resistors in the circuit.
     *****************************************************************/
    public int getResistorCount() { return numResistors; }

    /*****************************************************************
     * Returns the number of capacitors in the circuit.
     *****************************************************************/
    public int getCapacitorCount() { return numCapacitors; }

    /*****************************************************************
     * Returns the number of transistors in the circuit.
     *****************************************************************/
    public int getTransistorCount() { return numTransistors; }

    /*****************************************************************
     * Returns the number of diodes in the circuit.
     *****************************************************************/
    public int getDiodeCount() { return numDiodes; }

    /*****************************************************************
     * Returns the number of unique nodes in the Circuit
     *****************************************************************/
    public int getNodeCount() { return nodeMap.size(); }

    /*****************************************************************
     * Get the number of voltage sources in the system
     *****************************************************************/
    public int getVSourceCount() { return numVSources; }

    /*****************************************************************
     * Get the number of voltage sources in the system
     *****************************************************************/
    public int getISourceCount() { return numISources; }
   
    /** Used to include subcircuit elements in counts **/
    private void updateCounts(JaspiceCircuit subc) {
        this.numTransistors += subc.getTransistorCount();
        this.numCapacitors  += subc.getCapacitorCount();
        this.numDiodes      += subc.getDiodeCount();
        this.numResistors   += subc.getResistorCount();
        this.numVSources    += subc.getVSourceCount();
        this.numISources    += subc.getISourceCount();
    }

    /** Flatten this cell (remove all of its subcells and incorporate them
     * into the circuit)**/
    public void flatten() {
        for(Iterator subcells = getSubcellNames();subcells.hasNext();) {
            String subname = (String) subcells.next();
            JaspiceCircuit subc =
                (JaspiceCircuit) getSubcellDefForName(subname);
            //Must be flat itself to be added in this manner
            subc.flatten();
            updateCounts(subc);
            HierName hsubname = HierName.makeHierName(subname);
            //loop through the nodes, add all of its aliases from the subcell
            //Then find the pointer to that node in the flattened circuit
            for (Iterator nodes = subc.getNodes().iterator(); nodes.hasNext();) {
                Node node = (Node) nodes.next();
                HierName new_name = HierName.prefixName(hsubname,node.getName());
                for ( Iterator aliases = subc.getAliases(node);
                      aliases.hasNext();) {
                    HierName alias = HierName.prefixName(hsubname,
                                                        (HierName)aliases.next());
                    //bring in the new aliases as long as its not a node pointer
                    if (!alias.isPointer() )//&& !alias.equals(new_name))
                        aliasNames(new_name, alias);
                }
                //Now find the pointer for this node
                boolean found = false;
                for ( Iterator equiv = getEquivalentNames(new_name);
                      equiv.hasNext();) {
                    HierName hn = (HierName) equiv.next();
                    if ( hn.isPointer()) {
                        found = true;
                        break;
                    }
                }
                //add the node if not found
                if (!found) processNode(new_name);
            }

            //Now loop through the devices, changing all of the names and
            //reassigning the node dependancies
            for (Iterator devices = subc.getDevices().iterator();
                    devices.hasNext();) {
                AbstractDevice device = ((AbstractDevice) devices.next());
                Node[] nodes = device.getNodes();
                Node[] new_nodes = new Node[nodes.length];
                for(int i=0;i<nodes.length;i++) {
                    HierName new_name = HierName.prefixName(hsubname, nodes[i].getName());
                    Node ptr = findPointer(new_name);
                    new_nodes[i] = ptr;
                }
                HierName new_dev_name =
                    HierName.prefixName(hsubname, device.getName());
                try {
                    addDevice( device.copy(new_dev_name, new_nodes));
                } catch (AbstractDevice.AbstractDeviceException e) {
                    System.out.println(e.getMessage());
                }
            }
        }
        removeAllSubcells();
    }       
        

            
    /** Looks in the nodeMap for the node with name <code>name</code> **/
    public Node findNode(String name) {
        try {
            HierName hname = HierName.makeHierName(name,'.');
            return findNode(hname);
        } catch (InvalidHierNameException e) {
            return null;
        }
    }

    public Node findNode(HierName hname) {
        Node node = (Node) nodeMap.get(hname);
        if (node != null) return node;
        //not canonical, gotta go searching
        return findPointer(hname);
    }

    /** Find the nodename that actually points to a real node **/
    private Node findPointer(HierName name) {
        Iterator i = getEquivalentNames(name);
        if (i == null) return null;
        while (i.hasNext()) {
            HierName curr = (HierName) i.next();
            if (curr.isPointer()) {
                //Get the pointer
                return (Node) nodeMap.get(HierName.trim(curr));
            }
        }
        System.out.println("... not found.");
        return null;
    }

    public AbstractDevice findDevice(String name) {
        HierName hname = HierName.makeHierName(name);
        return findDevice(hname);
    }

    public AbstractDevice findDevice(HierName hname) {
        return (AbstractDevice) devices.get(hname);
    }
  
    public Collection getNodes() { return nodeMap.values(); }

    public Collection getDevices() { return devices.values(); }

    /**
     * Take one simulation timestep, returns the number of iterations used in the
     * step.
     **/
    public long step() {
        initialize();
        long iterations = analogStep();
        time += timestep;
        return iterations;
        
    }

    /** Initialize all nodes in the circuit **/
    private void initialize() {
        // Clear out data values
        for (final Iterator i = nodeMap.values().iterator(); i.hasNext(); )
            ((Node) i.next()).initialize();
    }    


    /** Set the dsim time internally in Jaspice.  Because Jaspice is timesteped,
     * and DSim only cycles when it has inputs, Jaspice needs to keep an 
     * internal count on the amount it needs to stroke DSim to stay synchronized
     * **/
    public static void setDSimTime(long dtime) {
        dsimtime = dtime;
    }

    /**
     * Stroke the dsim so that it is in step with Jaspice, will only run
     * with a positive dsimastep
     **/
    public static void digitalStep(long dsimstep) {
        if (dsimstep > 0) {
            dsimtime += dsimstep;
            DSim dsim = DSim.get();
            dsim.cycle(dsimtime - dsim.getTime());
        }
    }
        
    /**
     * Take one simulation step for analog devices.
     **/
    private int analogStep() {
        evaluateDevices();
        int iters = evaluateNodes();
        changeVoltage();
        return iters;
    }
   
    private void evaluateDevices() {
        // Evaluate voltages, charges and currents on all devices
        for (final Iterator i = devices.values().iterator(); i.hasNext(); ) {
            AbstractDevice device = (AbstractDevice) i.next();
            device.evalVoltage(chargeScale, currentScale,
                               derivChargeScale, derivCurrentScale,time);
        }
    }

    private int evaluateNodes() {
        final Collection c = nodeMap.values();
        //
        //Evaluate all of the drivers on the nodes, such as digital drivers
        //
        for (final Iterator i = c.iterator(); i.hasNext(); ) {
            Node node = (Node) i.next();
            if (node.getDriver() != null) {
                node.setAnalogNode(node.getDriver().getDrivenVoltage(time),
                                   chargeScale, currentScale,
                                   derivChargeScale, derivCurrentScale,
                                   timestep,
                                   node.getDriver().getDrivenTau());
            }
        }

        //
        //Normalize the nodes (makes AX=B simpler)
        //
        for (final Iterator i = c.iterator(); i.hasNext(); )
            ((Node) i.next()).normalize();
/*
        for (final Iterator i = c.iterator(); i.hasNext(); ) {
            Node node = (Node) i.next();
            System.out.print(node.getName()+" v= "+node.getVoltage()+" delv= "+node.getVoltageChange()+" B="+node.getResult()+" : ");
            TreeMap neighborMap = node.getNeighborMap();
            for (final Iterator j = neighborMap.keySet().iterator();
                     j.hasNext(); ) {
                Object nodeId = j.next();
                if (nodeId.equals(node.getHash()))
                    continue;

                double[] matrixValue = (double[]) neighborMap.get(nodeId);
                //Node neighbor = null;
                //if (changeGlobals)
                //     node = getNode((HierName) nodeId);
                //else node = findNode((HierName) nodeId);
                Node neighbor = (Node) nodeMap.get(nodeId);
                System.out.print(neighbor.getName()+"("+matrixValue[0]+") ");
            }
            System.out.println("");
        }      
*/
        
        //GGG Is it really good to directly access neighborMap or not?
        // Use relaxation to solve for new voltage changes
        double maxDelta;
        //Count the number of relaxation steps used
        int curr_iterations = 0;
        //Damp if needed
        double alpha = 1;
        do {
            // Greatest change in X, where X is estimated change in voltage
            maxDelta = 0;

            // Relax each individual node
            // In theory, we want A*X = B
            // We can write this as Bi - Sum(all j but i){Aij*Xj} - Aii*Xi = 0
            // So Xi = (Bi - Sum(all j but i){Aij*Xj})/Aii
            for (final Iterator i = c.iterator(); i.hasNext(); ) {
                Node node = (Node) i.next();
                Object nodeId = node.getHash();
                TreeMap neighborMap = node.getNeighborMap();

                // Iterate over all neighboring nodes
                double change = node.getResult();
                for (final Iterator j = neighborMap.keySet().iterator();
                     j.hasNext(); )
                {
                    Object neighId = (Object) j.next();
                    if (neighId.equals(nodeId))
                        continue;

                    double[] matrixValue = (double[]) neighborMap.get(neighId);
                    Node neighbor = (Node) nodeMap.get(neighId);
                    change -= matrixValue[0] * neighbor.getVoltageChange();
                }

                double changeDelta = Math.abs(change - node.getVoltageChange());
                node.setVoltageChange(alpha*change); //damp with alpha
                //The total change needs to be communicated to the friend nodes

                if (changeDelta > maxDelta)
                    maxDelta = changeDelta;
            }
            curr_iterations++;
            if (curr_iterations == 1000) {
                System.out.println("WARNING: relaxation hasn't converged in 1000"+
                                   " iterations, maxDelta= "+maxDelta+
                                   " damping... ");
            }
        } while (maxDelta > relaxationThreshold);
        if (curr_iterations >=1000) System.out.println("successful.");
        return curr_iterations;
    }
    
    public void changeVoltage() {
        Collection c = nodeMap.values();
        // Save node change values
        for (final Iterator i = c.iterator(); i.hasNext(); )
            ((Node) i.next()).changeVoltage();
    }

    /*************************************************
     * Circuit Statistics
     *************************************************/

    public void printStats() {
        System.out.println("Circuit Statistics:\n"+
                           "  "+getNodeCount()+" analog nodes\n"+
                           "  "+getTransistorCount()+" transistors"+
                           ", "+getDiodeCount()+" diodes\n"+
                           "  "+getCapacitorCount()+" capacitors"+
                           ", "+getResistorCount()+ " resistors\n"+
                           "  "+getISourceCount()+" current sources"+
                           ", "+getVSourceCount()+" voltage sources");
    }

    /*************************************************
     * Static subcircuit repository nested subclass.
     *************************************************/
    public static class Repository extends AbstractCircuit.Repository {
        
        /**
         * CircuitGraph generator method.
         **/
        protected AbstractCircuit newCircuitGenerator(final String cellType) {
            return new JaspiceCircuit(cellType);
        } 
    }

    /*****************************************************************
     * Class to represent exceptions in CircuitGraph processing.
     *****************************************************************/
    public static final class JaspiceCircuitException extends java.lang.Exception {
        public JaspiceCircuitException(final String message) {
            super(message);
        }
    }

}

