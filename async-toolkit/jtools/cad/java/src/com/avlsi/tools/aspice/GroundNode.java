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

import com.avlsi.file.common.HierName;
/**
 * This class simulates the ground node
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class GroundNode extends Node {
//GGG I feel bad implementing this class by "pasting over" 4 functions
// with nothing functions.  But I don't think devices should know or
// care if they talk to a ground node... so the ground node needs to
// support some sort of null API for consistancy with other nodes.
// Is this the best way of writing things?  Remember that we need a
// coherent node numbering scheme, so we can't use an interface.
//
// Actually, if we never need to lookup the ground node in a tree,
// we wouldn't need the node ID to also track ground, so that could
// enable us to use an interface...
    /** Named GroundNode
     ***/
    public GroundNode() {
        super();
    }

    public GroundNode(HierName name) {
        super(name);
        //System.out.println("(Ground)");
    }
    
    /**
     * Track neighbor nodes.
     * The ground node does not care which nodes use it because its
     * voltage is always zero.
     * @param nodes list
     **/
    protected void addNeighborNodes(Node[] nodes) {
    }

    /**
     * Set Voltage.
     * Since this is the ground node, its voltage doesn't change
     * @param voltage to set (for consistancy)
     **/
    public void setVoltage(double voltage) {
    }

    /**
     * Compute and set result value.
     * We don't need to do any voltage calculation for the ground node
     * @param chargeScale scalar for charge
     * @param charge on node
     * @param currentScale scalar for current
     * @param current on node
     **/
    public void setResult(double chargeScale, double charge,
                          double currentScale, double current)
    {
    }

    /**
     * Compute and set matrix entry.
     * We don't need to do any voltage calculation for the ground node
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
    }

    /**
     * Ground nodes don't need normalization
     **/
    public void normalize() {
    }
}
