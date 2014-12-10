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

import java.util.Iterator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.TreeMap;
import com.avlsi.file.common.HierName;

/**
 * This class simulates a circuit node
 *
 * @author Ted Vessenes, extended by Dan Daly
 * @version $Name:  $ $Date$
 *
 * In theory, A*X = B where A is a matrix containing information
 * about partial derivatives of current and charge (with respect
 * to voltage) of all nodes in the system.  neighborNodeMatrixRow
 * represents a row of this sparse matrix.  It is a list of all matrix
 * entries based on derivatives with respect to this node's voltage.
 * So if this is node 0, then neighborMap.get(node_1) contains data
 * based on dI1/dV0 and dQ1/dV0.  Note that X is a vector of current
 * voltages, per node.  This value is represented by "voltage".  (The
 * entire vector is distributed across all nodes.)  B is a vector of
 * information based on I and Q (cv. matrix row based on dI and dQ).
 * Like X, B is also distributed across all nodes.  Its data value is
 * kept in "result".
 **/
public class Node implements java.io.Serializable{
    
    public static final int VOLTAGE  = 0;

    /** Id of the last created node. **/
    private static int lastId = 0;

    /** Id of this node. **/
    private Integer id;

    /** Hash Object for this Node **/
    private Object hash;
    
    /** Name of this node (optional) **/
    private HierName name;
    /**
     * A row of a sparse Matrix associated with this node.
     * Based on Current and Charge derivatives (dI and dQ) with
     * respect to this node's voltage.
     * See class comments for more information.
     **/
    private TreeMap neighborMap;

    /**
     * Voltage across node.
     * See class comments for more information.
     **/
    private double voltage;

    /** Change in voltage since relaxation (performed by circuit). **/
    private double voltageChange;

    /**
     * Result vector B of A*X=B.
     * Based on Current and Charge data (I and Q) for this node.
     * See class comments for more information.
     **/
    private double result;
   
    /** Collection of watches on the node **/ 
    private final Collection watches = new ArrayList(1);
    
    /** The driver that drives the node **/
    private NodeDriver driver;
    
    /**
     * Construct new node.
     **/
    public Node() {
        this(0);
        name = null;
    }

    /**
     * Construct new node with initial voltage.
     * @param voltage of node
     **/
    public Node(double voltage) {
        id = new Integer(++lastId);
        neighborMap = new TreeMap();
        this.driver = null;

        this.voltage = voltage;
        this.voltageChange = 0;
        this.hash = getId();
    }

    public Node(HierName name) {
        this(0);
        this.name = name;
        //System.out.println("New Node: "+name);
    }

    public Node(HierName name,double voltage) {
        this(voltage);
        this.name = name;
        //System.out.println("New Node: "+name);
    }

    /**
     * Track neighbor nodes.
     * Devices using this node need to inform this node who its
     * neighbors are.  After all, neighbors are defined as "all nodes
     * connected to this node through at most one device."
     * @param nodes list
     **/
    protected void addNeighborNodes(Node[] nodes) {
        for (int i = 0; i < nodes.length; i++) {
            Object neighborId = nodes[i].getHash();

            if (!(nodes[i] instanceof GroundNode) &&
                !neighborMap.containsKey(neighborId))
                neighborMap.put(neighborId, new double[] {0}); }
    } 

    /**
     * Add device to node.
     * Note that we don't initialize the voltage because this speeds up
     * relaxation convergence.
     * @param device to add
     **/
    protected void initialize() {
        result = 0;

        for (final Iterator i = neighborMap.values().iterator(); i.hasNext(); )
            ((double[]) i.next())[0] = 0; // Set A = 0
    }

    /**
     * Get Node Id.
     * @return Id of this node.
     **/
    public Integer getId() {
        return id;
    }

    /**
     * Calculate hash code
     * @return hash code (id of node)
     **/
    public int hashcode() {
        return id.intValue();
    }

    public Object getHash() {
        return hash;
    }

    public void setHash(Object o) {
        hash = o;
    }

    /**
     * Get Node Name
     * @return name
     **/
    public HierName getName() { return name; }
    /**
     * Get Voltage.
     * @return voltage
     **/
    public double getVoltage() {
        return voltage;
    }

    /** Send voltage to digital node **/
    private void sendVoltage() {
        for (Iterator i = watches.iterator(); i.hasNext();) {
            AnalogWatcher nw = (AnalogWatcher)i.next();
            nw.voltageChanged(voltage);
        }
    }

    /**
     * Set Voltage.
     * @param voltage to set
     **/
    public void setVoltage(double voltage) {
        this.voltage = voltage;
        sendVoltage();
    }

    /**
     * Change Voltage
     **/
    public void changeVoltage() {
        this.voltage += voltageChange;
        sendVoltage();
       
        
    }

    /**
     * Get Voltage Change
     **/
    public double getVoltageChange() {
        return voltageChange;
    }

    /**
     * Set Voltage Change
     **/
    public void setVoltageChange(double voltageChange) {
        this.voltageChange = voltageChange;
    }

    /**
     * Get Result (B in A*X = B)
     * @return result value
     **/
    public double getResult() {
        return result;
    }

    /**
     * Compute and set result value.
     * @param chargeScale scalar for charge
     * @param charge on node
     * @param currentScale scalar for current
     * @param current on node
     **/
    public void setResult(double chargeScale, double charge,
                          double currentScale, double current)
    {
        result += chargeScale*charge + currentScale*current;
    }

    public void forceResult(double d) {
        result = d;
    }

    /**
     * Get map of neighbors to sparse matrix row entry
     * @return node matrix row
     **/
    public TreeMap getNeighborMap() {
        return neighborMap;
    }

    /**
     * Compute and set matrix entry.
     * @param node if charge derivative is dI0/dV1, node is 0 (and "this" is 1)
     * @param chargeScale scalar for charge
     * @param charge derivative on node
     * @param currentScale scalar for current
     * @param current derivative on node
     **/
    public void setMatrix(Node node,
                          double chargeScale, double charge,
                          double currentScale, double current)
    {
        Object nodeId = node.getHash();
        double[] matrixValue = (double[]) neighborMap.get(nodeId);
        if (matrixValue != null)
            matrixValue[0] += chargeScale*charge + currentScale*current;
    }

    /**
     * Normalize Matrix row and result values for faster computation
     **/
    public void normalize() {
        double divisor = ((double[]) neighborMap.get(getHash()))[0];
        if (divisor == 0)
            System.out.println("Warning: Attempting to normalize zero row!");

        for (final Iterator i = neighborMap.values().iterator(); i.hasNext(); )
            ((double[]) i.next())[0] /= divisor;

        result /= divisor;
    }
    
    /** Remove from our watcher list. **/
    public synchronized boolean removeAnalogWatcher(AnalogWatcher w) {
        return watches.remove(w);
    }

    /** Add to our watcher list, for change notifications. **/
    public synchronized boolean addAnalogWatcher(AnalogWatcher w) {
        return watches.add(w);
    }
    /** Check presence in our watcher list. **/
    public synchronized boolean isAnalogWatcher(AnalogWatcher w) {
        return watches.contains(w);
    }

    public interface NodeDriver {
        double getDrivenVoltage(double time);
        double getDrivenTau();
    }

    public synchronized void setDriver(NodeDriver driver) {
        this.driver = driver;
    }

    public NodeDriver getDriver() {
        return driver;
    }
    
    /** set an analog node to V, ex values: mAq=1, mAi=-0.5*timestep,
     *  mBq=0, mBi=timestep */
    public void setAnalogNode(double V, double chargeScale, double currentScale, 
                              double derivChargeScale, double derivCurrentScale,
                              double timestep,double prstau) {
        double tau = prstau;
        if (tau < 4*timestep) tau = 4*timestep;
        //this.setResult(mBq*tau*(this.getVoltage() - V) -
        //               mBi*(this.getVoltage() -V));
        result = (chargeScale*tau-currentScale)*(this.getVoltage() - V);
        /*  Once a node becomes 'digital' or driven to the voltage V, 
         *  it cannot be influenced by its neighboring nodes.  So the 
         *  diagonal matrix element is set to mAq*tau - mAi
         *  */
        for (final Iterator i= this.neighborMap.keySet().iterator();
                i.hasNext();) {
            Object nodeId = i.next();
            double[] matrixValue = (double[]) neighborMap.get(nodeId);
            if (nodeId.equals(getHash())) {
                   matrixValue[0] = derivChargeScale*tau - derivCurrentScale;
            } else matrixValue[0] = 0;
        }
    }
        
}
