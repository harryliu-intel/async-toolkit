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

import java.util.HashMap;
import java.util.Iterator;

import com.avlsi.file.common.HierName;

import com.avlsi.util.recalc.ExpressionInterface;
import com.avlsi.util.recalc.IndependantVariable;

/**
 * Class to represent current sources
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public final class CurrentSource extends AbstractDevice {
    /** Node that receives the current. **/
    private Node source;

    /** Expression to evaluate current for source. **/
    private ExpressionInterface currentFormula;

    /**
     * Map whose values are independant variables used by currentFormula
     * and keys are nodes that influence those variables.
     **/
    private HashMap voltageMap;

    /**
     * Class constructor.
     * @param source node to output current
     * @param currentFormula Recalc expression describing current output
     * @param voltageMap Maps independant variables in currentFormula with
     * nodes.  See class variable voltageMap for more information
     **/
    public CurrentSource(HierName name, Node source, 
                         ExpressionInterface currentFormula,
                         HashMap voltageMap)
    {
        super(new Node[] { source });
        this.currentFormula = currentFormula;
        this.voltageMap = voltageMap;
        this.name = name;
    }
    
    public CurrentSource(Node source, 
                         ExpressionInterface currentFormula,
                         HashMap voltageMap) {
        this(null, source, currentFormula, voltageMap);
    }        

    /**
     * Get source node.
     * @return source node
     **/
    public Node getSource() {
        return nodes[0];
    }

    public String getCode() { return "I";}
    
    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device and then informs its nodes of these values.
     * @param chargeScale scalar for charge calculation
     * @param currentScale scalar for current calculation
     * @param derivChargeScale scalar for charge derivative calculation
     * @param derivCurrentScale scalar for current derivative calculation
     **/
    public void evalVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time)
    {
        Node source = getSource();

        // Initialize formula variables with voltages from influencing nodes
        for (final Iterator i = voltageMap.keySet().iterator(); i.hasNext(); ) {
            Node node = (Node) i.next();
            IndependantVariable var =
                (IndependantVariable) voltageMap.get(node);
            var.set(node.getVoltage());
        }

        double current = currentFormula.eval();
        source.setResult(chargeScale, 0, currentScale, current);

        // Compute partial derivatives
        final double delta = 1e-9;
        for (final Iterator i = voltageMap.keySet().iterator(); i.hasNext(); ) {
            Node node = (Node) i.next();
            IndependantVariable var =
                (IndependantVariable) voltageMap.get(node);

            final double oldVoltage = var.get();
            var.set(oldVoltage + delta);
            double dIdV = (currentFormula.eval() - current) / delta;

            source.setMatrix(node, derivChargeScale, 0,
                                   derivCurrentScale, dIdV);

            var.set(oldVoltage);
        }
    }
}
