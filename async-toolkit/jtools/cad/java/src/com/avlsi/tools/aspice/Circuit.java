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
import java.util.TreeMap;

/**
 * This class simulates a circuit
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class Circuit {
    /** List of AbstractDevice objects in the circuit. **/
    private ArrayList devices = new ArrayList();

    /** List of Node objects in the circuit. **/
    private TreeMap nodes = new TreeMap();

    /** Weighting constant for charge data. **/
    private double chargeScale;

    /** Weighting constant for current data. **/
    private double currentScale;

    /** Weighting constant for charge derivative data. **/
    private double derivChargeScale;

    /** Weighting constant for current derivative data. **/
    private double derivCurrentScale;

    /** Threshold for relaxation voltage change. **/
    private static final double relaxationThreshold = 1e-6;

    /** Amount of time lapsed in one timestep. **/
    private double timestep;

    /** Current simulation time. **/
    private double time;

    /**
     * Construct default circuit
     **/
    public Circuit() {
        this(1e-12);
    }
//GGG Create constructor based on cell object.

    /**
     * Construct blank circuit.
     * @param timestep for simulation increments
     **/
    public Circuit(double timestep) {
        this.timestep = timestep;
        this.time = 0;

        this.chargeScale = 0;
        this.currentScale = timestep;
        this.derivChargeScale = 1;
        this.derivCurrentScale = -timestep/2;
    }

    /**
     * Add device to circuit.
     * @param device to add
     **/
    public void addDevice(AbstractDevice device) {
        devices.add(device);

        Node[] deviceNodes = device.getNodes();
        for (int i = 0; i < deviceNodes.length; i++)
            addNode(deviceNodes[i]);
    }

    /**
     * Add node to circuit.
     * Note that addDevice uses this function to implicitly add that
     * device's nodes.
     * @param node to add
     **/
    public void addNode(Node node) {
        Integer nodeId = node.getId();
        if (!nodes.containsKey(nodeId))
            nodes.put(nodeId, node);
    }

    /**
     * Initialize circuit for simulation
     **/
    public void initializeSimulation() {
        time = 0;

        for (final Iterator i = nodes.values().iterator(); i.hasNext(); ) {
            Node node = (Node) i.next();
            node.setVoltage(0);
        }
    }

    /**
     * Default simulation of circuit.
     **/
    public void simulate() {
        initializeSimulation();
        jump(50e-9);
    }

    /**
     * Take multiple simulation steps
     * @param timeChange number of seconds to step forward
     **/
    public void jump(double timeChange) {
        double stopTime = time + timeChange;

        while (time < stopTime)
            step();
    }

    /**
     * Take multiple simulation steps
     * @param steps number of steps to take
     **/
    public void jump(int steps) {
        // JVM hangs here because 0 < steps => false for for all step values!
        //while (0 < steps--)
        while (steps-- > 0)
            step();
    }

    /**
     * Take one simulation timestep.
     **/
    public void step() {
        // Clear out data values
        for (final Iterator i = nodes.values().iterator(); i.hasNext(); )
            ((Node) i.next()).initialize();

        analogStep();
//GGG Add in digital Step sometime.  Add it before or after analog?

        time += timestep;
    }

    /**
     * Take one simulation step for analog devices.
     **/
    private void analogStep() {
        // Evaluate voltages, charges and currents on all devices
        for (final Iterator i = devices.iterator(); i.hasNext(); ) {
            AbstractDevice device = (AbstractDevice) i.next();
            device.evalVoltage(chargeScale, currentScale,
                               derivChargeScale, derivCurrentScale,time);
        }

        for (final Iterator i = nodes.values().iterator(); i.hasNext(); )
            ((Node) i.next()).normalize();

//GGG Is it really good to directly access neighborMap or not?
        // Use relaxation to solve for new voltage changes
        double maxDelta;
        do {
            // Greatest change in X, where X is estimated change in voltage
            maxDelta = 0;

            // Relax each individual node
            // In theory, we want A*X = B
            // We can write this as Bi - Sum(all j but i){Aij*Xj} - Aii*Xi = 0
            // So Xi = (Bi - Sum(all j but i){Aij*Xj})/Aii
            for (final Iterator i = nodes.values().iterator(); i.hasNext(); ) {
                Node node = (Node) i.next();
                Integer id = node.getId();
                TreeMap neighborMap = node.getNeighborMap();

                // Iterate over all neighboring nodes
                double change = node.getResult();
                for (final Iterator j = neighborMap.keySet().iterator();
                     j.hasNext(); )
                {
                    Integer nodeId = (Integer) j.next();
                    if (nodeId.intValue() == id.intValue())
                        continue;

                    double[] matrixValue = (double[]) neighborMap.get(nodeId);
                    Node neighbor = (Node) nodes.get(nodeId);

                    change -= matrixValue[0] * neighbor.getVoltageChange();
                }

                double changeDelta = Math.abs(change - node.getVoltageChange());
                node.setVoltageChange(change);

                if (changeDelta > maxDelta)
                    maxDelta = changeDelta;
            }
        } while (maxDelta > relaxationThreshold);

        // Save node change values
        for (final Iterator i = nodes.values().iterator(); i.hasNext(); )
            ((Node) i.next()).changeVoltage();
    }

    /**
     * Get current simulation time
     * @return simulation time in seconds
     **/
    public double getTime() {
        return time;
    }
}
